import pymorphy2
import json
import ast
from tqdm import tqdm

morph = pymorphy2.MorphAnalyzer()

all_dict = {}

with open('all_dict.json', 'r', encoding='utf-8') as f:
    all_dict = json.load(f)

with open('pym_to_synt/pymorphy_to_syntagrus.json', 'r', encoding='utf-8') as f:
    pts_m = json.load(f)

s_tags = list(set([item for sublist in pts_m.values() for item in sublist]))

dict_file = 'pym_to_synt/sirija_dict.txt'


def to_syntagrus(word):
    l = morph.parse(word)
    tags = set()
    
    for tag in l:
        t = set(tag.tag.grammemes)
        tags.update(t)

    res = []
    for tag in tags:
        if tag in pts_m:
            res.append(pts_m[tag])
    return set([item for sublist in res for item in sublist])


# s является подмножеством gramms
def check_word(s, gramms):
    if len([i for i in gramms if i not in s]) == 0:
        return True
    return False


# проверяет словарь корпуса синтагруса
# ключ - набор граммем
# значение - слова, соответствующие этому набору граммем
# переводит все слова отображением и проверяет является ли набор граммем(ключ) подмножеством этого отображения
def check_dict():
    errs = 0
    for key, val in all_dict.items():
        gramms = key.split(' ')
        for word in val:
            s = to_syntagrus(word)
            if not check_word(s, set(gramms)):
                errs += 1
                print(word)
                print([i for i in gramms if i not in s])
                l = morph.parse(word)
                tags = set()
                
                for tag in l:
                    t = set(tag.tag.grammemes)
                    tags.update(t)
                print(f'pymorphy - {l}')
                print(f'pymorphy tags - {tags}')
                print(f'to_syntagrus - {s}')
                print(f'gramms - {gramms}\n')
    print(f'total errors amount - {errs}')


# проходится по файлу со словами, которых нет в синтагрусе
# строит для каждого слова отображение в граммемы синтагруса
# ищет в словаре синтагруса слова, набор граммем которых является подмножеством построенного отображения
def check_difference():
    all_words = []
    with open('json_generator/open_corpora_difference.txt', 'r', encoding='utf-8') as f:
        all_words = ast.literal_eval(f.read())
    not_found = set()
    similar = {}
    for word in all_words:
        s = to_syntagrus(word)
        b = False # если b внутри цикла стал True, то это слово, с подмножеством граммем
        for key, val in all_dict.items():
            gramms = key.split(' ')
            b = b or check_word(s, set(gramms))
            if b:
                # закомментированы строчки, которые добавляют к каждому слову из словаря его граммемы
                #! файл становится очень большим
                # чтобы работало, нужно расскоментировать и поменять в similar[word] ниже val на val_set
                #val_set = val
                #for i, v in enumerate(val_set):
                #    val_set[i] = v + f'[{key}]'
                
                if word in similar:
                    similar[word].update(set(val))
                else:
                    similar[word] = set(val)
                break
        if not b: 
            not_found.add(word)   
        
    if len(not_found) != 0:
        print(f'not found similar words in syntagrus of {len(not_found)} open corpora words')

    with open('pym_to_synt/open_corpora_similar.txt', 'w', encoding='utf-8') as f:
        for key, val in similar.items():
            key_gramms = to_syntagrus(key)
            print(f"{key}{list(key_gramms)} - {val}", file=f)
         

#check_dict()
#check_difference()
#l = morph.parse('ой')
#tags = set()
#print(l)
# составление множества всех тегов, которые выдал pymorphy
#for tag in l:
#    
#    t = set(tag.tag.grammemes)
#    tags.update(t)