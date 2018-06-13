import csv
from mtranslate import translate

# pip install mtranslate

language = 'ca'

with open('emote.csv', 'r') as csv_file:
    reader = csv.reader(csv_file)
    en_words = [word[0] for word in reader]

translated_words = [translate(word, language) for word in en_words]

with open('%s_emote.csv' % language, 'w', newline='') as csv_file:
    writer = csv.writer(csv_file)
    for row in translated_words:
        writer.writerow([row])
