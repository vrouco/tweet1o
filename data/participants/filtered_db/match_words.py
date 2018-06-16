import csv
import difflib
from unidecode import unidecode


def read_csv(csv_file, delimiters):
    dialect = csv.Sniffer().sniff(csv_file.read(), delimiters=delimiters)
    csv_file.seek(0)
    reader = csv.reader(csv_file, dialect=dialect)
    return reader


country = 'spain'

with open('%s1-0_corr.csv' % country, 'r', encoding='latin_1') as csv_file:
    tweets_info = [row for row in read_csv(csv_file, ';,')]


with open('hinojosa_3_lang.csv', 'r', encoding='utf-8') as csv_file:
    reader = csv.reader(csv_file)
    hin_dict = [row[0] for row in reader]

'''
for i, row in enumerate(tweets_info):
    sentence = row[2].replace('.', '')
    words = sentence.split()
    words = [unidecode(word.lower()) for word in words]
    tweet = [row[0], words]
    tweet_words.append(tweet)
'''

# print(tweet_words)
# print(cat_dict)

threshold = 0.86

for n, row in enumerate(tweets_info[1:]):
    sentence = row[2].replace('.', '')
    twords = sentence.split()
    twords = [unidecode(word.lower()) for word in twords]
    indian_tweet = []
    for x, tword in enumerate(twords):
        new_word = ''
        for i, dword in enumerate(hin_dict):
            seq_match = difflib.SequenceMatcher(None, tword, dword)
            if seq_match.ratio() > threshold:
                new_word = dword
                # print(tword, dword, [new_word])
                break
            else:
                new_word = tword
                # print(new_word)
        indian_tweet.append(new_word)
        # print(indian_tweet)
    row[2] = ' '.join(indian_tweet)
    # print(indian_tweet)
    print(row)

with open('%s1-0_indian.csv' % country, 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerows(tweets_info)
