import csv
import difflib
from unidecode import unidecode

rows = []

country = 'spain'

with open('%s1-0_corr.csv' % country, 'r', encoding='latin_1') as csv_file:
    reader = csv.reader(csv_file, delimiter=',')
    for row in reader:
        rows.append(row)

tweet_words = []

for i, row in enumerate(rows):
    sentence = row[2].replace('.', '')
    words = sentence.split()
    words = [unidecode(word.lower()) for word in words]
    tweet = [row[0], words]
    tweet_words.append(tweet)

print(tweet_words)

threshold = 0.85

for tweet in tweet_words:
    for n, tword in enumerate(tweet[1]):
        for i, dword in enumerate(dictionary):
            seq_match = difflib.SequenceMatcher(None, tword, dword)
            if seq_match.ratio() > threshold:
                print(tword, dword)


'''
match_cnstrs = []
threshold = 0.85
for i, c in enumerate(f1_cnstrs):
    for n, s in enumerate(f1_cnstrs):
        if i != n:
            seq_x = difflib.SequenceMatcher(None, c[0], s[0])
            if seq_x.ratio() > threshold:
                seq_y = difflib.SequenceMatcher(None, c[1], s[1])
                if seq_y.ratio() > threshold:
                    if sorted([i, n]) not in match_cnstrs:
                        match_cnstrs.append(sorted([i, n]))
                    print(c, s)
            # It's possible to invert the constructs but not worth it

print(len(match_cnstrs))

remove_ind = [cnstr[1] for cnstr in match_cnstrs]
print('INDEX')
print(len(set(remove_ind)))
f2_cnstrs = []
for i, cnstr in enumerate(f1_cnstrs):
    if i not in remove_ind:
        f2_cnstrs.append(cnstr)

print(len(f2_cnstrs))

with open('sorted_dil.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(sorted(f2_cnstrs))
'''