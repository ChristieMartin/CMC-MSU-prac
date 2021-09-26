'''
Используемые библиотеки: nltk, pymorphy2, collections
Используемые инструменты:
1. RegexpTokenizer для разбиентия текста на токены по шаблону
2. stopwords - множество стоп-слов
3. RegexpTokenizer.tokenize - разбиение на токены
4. MorphAnalyzer().parse - парсинг всевозможных форм слова, для поиска основы необходимо normal_form
5. Counter() - словарь с в элементе которого (слово, его_количество_вхождений_в_заданный_список)
6. most_common - метод Counter() сортирующий словарь по количеству вхождений слова в текст

Алгоритм:
1. Разбиваем текст на слова по шаблону регулярного выражения, представляющего из себя просто слово. Полученные слова хранятся в списке 
2. Удаляем из списка слов стоп-слова
3. Ищем основу для каждого слова из полученного списка
4. Считаем в полученном списке для каждого элемента количество его вхождений в этот список и выводим

Минус программы: захватываются также и иностранные слова(пробовал использовать words из nltk.corpus, но он сделан только для английских слов)
'''

from nltk.corpus import stopwords
from nltk.tokenize import RegexpTokenizer
from pymorphy2 import MorphAnalyzer
from collections import Counter
f = open("text.txt", "r", encoding = "utf-8").read()

stop_words = set(stopwords.words('russian')) # создаем множество русских стоп-слов
tokenizer = RegexpTokenizer(r'\w+') # использую разбиение на токены по шаблону регулярного выражения(чтобы оставить только слова и убрать, например, пунктуацию)
tokens = tokenizer.tokenize(f) # разбиваю на токены
words = [w for w in tokens if not w in stop_words] # убираю из токенизированного списка стоп-слова

lematized = [MorphAnalyzer().parse(word)[0].normal_form for word in words] # ищу для каждого слова основу

c = Counter(lematized).most_common() # подсчет найденных элементов и сортировка их по убыванию
for i,j in enumerate(c):
    print(str(i + 1) + ".", j[0], "-", j[1]) # печать: слово + сколько оно встречается
    
# печать моей программы не отличается от использования инструмента
