import csv

# Before using this script is needed to delete all "" and '' from the csv

# ENCODINGS
# 'latin_1' = 'ISO-8859-1'
# á     Ã¡
# é     Ã©
# í     Ã­
# ó     Ã³
# ú     Ãº
# ñ     Ã±
# à     Ã
# è
# ì
# ò     Ã²
# ù
# Ç     Ã§

rows = []

country = 'spain'

with open('%s1-0_no_strings.csv' % country, 'r', encoding='latin_1') as csv_file:
    reader = csv.reader(csv_file, delimiter=';')
    for row in reader:
        rows.append(row)


def char_replace(sentence):
    charset = [['Ã¡', 'á'], ['Ã©', 'é'], ['Ã­', 'í'],
               ['Ã³', 'ó'], ['Ãº', 'ú'], ['Ã±', 'ñ'],
               ['Ã', 'à'], ['Ã²', 'ó'], ['Ã§', 'Ç']]
    for char in charset:
        sentence = sentence.replace(char[0], char[1])
    return sentence


def correct_row(rows):
    for n, row in enumerate(rows):
        if len(row) >= 23:
            new_row = []
            for i, v in enumerate(row):
                if i == 2:
                    new_row.append(v + row[3])
                elif i == 3:
                    pass
                else:
                    new_row.append(v)
            rows[n] = new_row


correct_row(rows)
correct_row(rows)

for i, row in enumerate(rows):
    if len(row) >= 3:
        row[2] = char_replace(row[2])
    if len(row) >= 23 or len(row) <= 3:
        del rows[i]

with open('%s1-0_corr.csv' % country, 'w', newline='', encoding='latin_1') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerows(rows)
