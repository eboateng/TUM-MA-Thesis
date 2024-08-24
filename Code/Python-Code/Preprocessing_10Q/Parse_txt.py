#!/usr/bin/env python
# coding: utf-8

# In[ ]:


pip install pymupdf


# In[3]:


import os
import random
import pandas as pd
import nltk
from langdetect import detect
from tqdm import tqdm
import re
import numpy as np


# In[ ]:



# Compiled regular expressions for efficiency
re_tab = re.compile(r"\t+")
re_pipe = re.compile(r"\|")
re_space = re.compile(r'\s+')

def extract_text_from_txt(txt_path):
    try:
        with open(txt_path, 'r', encoding='utf-8') as file:
            return file.read()
    except:
        return ""

def clean_rep_df(rep_df):
    rep_df["sentence"] = rep_df["sentence"].str.replace(re_tab, " ").str.replace(re_pipe, " ")
    rep_df = rep_df[~rep_df["sentence"].str.contains("www\.|WWW\.|file\\\\|^[\s+\t]+$", regex=True, case=False)]
    return rep_df[~rep_df.eq('').all(1)]

def remove_mainly_numeric_sentences(rep_df):
    return rep_df[rep_df["sentence"].apply(lambda x: sum(char.isdigit() for char in x) < sum(char.isalpha() for char in x))]

def get_raw_report(txt):
    sentences = nltk.sent_tokenize(txt.replace("\n", " "), language='english')
    return pd.DataFrame({"sentence": sentences})

def merge_sentences(report_df):
    if report_df.empty or 'sentence' not in report_df.columns:
        return pd.DataFrame(columns=['sentence'])
    txt = ' '.join(report_df["sentence"])
    sentences = nltk.sent_tokenize(txt)
    return pd.DataFrame({"sentence": sentences})

def drop_headline_rows(sentence):
    capslock_words = [word for word in sentence.split() if word.isupper() and len(word) >= 2]
    if len(capslock_words) >= 3:
        return np.nan
    else:
        return sentence

def remove_digit_at_start(df):
    df["sentence"] = [sent.strip() for sent in df["sentence"]]
    df["sentence"] = df["sentence"].apply(lambda x: x[1:].strip() if x and x[0].isdigit() else x)
    return df

def replace_multiple_spaces(sentence):
    return re_space.sub(' ', sentence)

def merge_wrong_splitted_sentences(df):
    new_sentences = []
    prev_sentence = ''

    for index, row in df.iterrows():
        current_sentence = row['sentence'].strip()
        if current_sentence and current_sentence[0].islower():
            prev_sentence = prev_sentence + ' ' + current_sentence
        else:
            if prev_sentence:
                new_sentences.append(prev_sentence)
            prev_sentence = current_sentence

    if prev_sentence:
        new_sentences.append(prev_sentence)

    return pd.DataFrame(new_sentences, columns=['sentence'])

def count_words(sentence):
    return len(sentence.split())

def parse_reports(main_path, res_path):
    for root, dirs, files in os.walk(main_path):
        for dir in dirs:
            os.makedirs(os.path.join(res_path, dir), exist_ok=True)

        for file in files:
            if file.endswith(".txt"):
                sub_path = os.path.relpath(root, main_path)
                res_folder = os.path.join(res_path, sub_path)
                os.makedirs(res_folder, exist_ok=True)

                res_file = os.path.join(res_folder, file[:-4] + ".csv")  # Change to .csv
                if not os.path.isfile(res_file):
                    file_path = os.path.join(root, file)
                    extracted_text = extract_text_from_txt(file_path)
                    if extracted_text:
                        report_df = get_raw_report(extracted_text)
                        if not report_df.empty:
                            report_df = merge_wrong_splitted_sentences(report_df)
                            report_df = clean_rep_df(report_df)
                            report_df = remove_mainly_numeric_sentences(report_df)
                            if not report_df.empty:
                                report_df = merge_sentences(report_df)
                                report_df["sentence"] = report_df["sentence"].apply(lambda x: drop_headline_rows(x))
                                report_df = report_df.dropna()  # drop rows where identified as headlines
                                report_df["sentence"] = report_df["sentence"].apply(lambda x: replace_multiple_spaces(x))
                                report_df = remove_digit_at_start(report_df)
                                report_df = merge_wrong_splitted_sentences(report_df)
                                # Add word count column
                                report_df['word_count'] = report_df['sentence'].apply(count_words)
                                # Save to CSV
                                report_df.to_csv(res_file, index=False, encoding='utf-8')

if __name__ == '__main__':
    print("Parsing reports...")
    parse_reports(main_path=r"C:\Users\ge27xix\SEC\LouMcd\10-X_C_1993-2000",
                  res_path=r"C:\Users\ge27xix\SEC\Processed10-X\Raw")


# In[ ]:





# In[ ]:




