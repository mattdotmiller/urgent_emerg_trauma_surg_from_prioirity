#!/usr/bin/env python3
"""
embed.py
[CLS] embeddings of the booking text (planned_procedure) and ACHI text
(achi_txt) using BioClinical-ModernBERT. Called by 03_matching_embeddings.R
and 07_matching_deploy.R.

Writes <prefix>emb_pp.npy, <prefix>emb_pt.npy and <prefix>labels.npy to the
working directory.

  python embed.py --input train_pairs.csv --prefix train_ --max_length 48
"""

import os
import gc
import argparse
import numpy as np
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModel
from tqdm import tqdm

os.environ["USE_TORCH"] = "1"
os.environ["USE_TF"] = "0"


def get_device():
    if torch.backends.mps.is_available():
        return "mps"
    if torch.cuda.is_available():
        return "cuda"
    return "cpu"


def cls_embeddings(texts, tokenizer, model, device, max_length, batch_size):
    out = []
    for i in tqdm(range(0, len(texts), batch_size), desc="Embedding"):
        enc = tokenizer(texts[i:i + batch_size], truncation=True, padding="max_length",
                        max_length=max_length, return_tensors="pt")
        with torch.no_grad():
            hidden = model(input_ids=enc["input_ids"].to(device),
                           attention_mask=enc["attention_mask"].to(device)).last_hidden_state
        out.append(hidden[:, 0, :].float().cpu().numpy())
        del enc, hidden
        gc.collect()
        if device == "mps":
            torch.mps.empty_cache()
    return np.vstack(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    ap.add_argument("--prefix", default="")
    ap.add_argument("--model", default="thomas-sounack/BioClinical-ModernBERT-base",
                    help="Hugging Face model name or local folder")
    ap.add_argument("--max_length", type=int, default=48)
    ap.add_argument("--batch_size", type=int, default=None)
    args = ap.parse_args()

    device = get_device()
    batch_size = args.batch_size or (32 if device in ("mps", "cuda") else 16)
    torch.set_num_threads(1 if device in ("mps", "cuda") else 4)

    df = pd.read_csv(args.input)
    tokenizer = AutoTokenizer.from_pretrained(args.model)
    model = AutoModel.from_pretrained(args.model, torch_dtype=torch.float32).to(device).eval()

    emb_pp = cls_embeddings(df["planned_procedure"].astype(str).tolist(), tokenizer, model,
                            device, args.max_length, batch_size)
    emb_pt = cls_embeddings(df["achi_txt"].astype(str).tolist(), tokenizer, model,
                            device, args.max_length, batch_size)
    labels = (df["correct"] == "yes").astype(int).values
    assert emb_pp.shape[0] == emb_pt.shape[0] == len(df), "Row alignment error"

    np.save(f"{args.prefix}emb_pp.npy", emb_pp)
    np.save(f"{args.prefix}emb_pt.npy", emb_pt)
    np.save(f"{args.prefix}labels.npy", labels)
    print(f"{len(df):,} rows embedded on {device}")


if __name__ == "__main__":
    main()
