from xml.dom.minidom import (parse, Element)
import json
import os
import glob
from tqdm import tqdm

class Sentence:
    def __init__(self, sent: Element):
        self.id = sent.getAttribute('ID')
        self.words = []
        
        self.parseWords(sent.getElementsByTagName('W'))
        
    def parseWords(self, rawWords):
        
        self.wordMap = {}
        
        for rawWord in rawWords:
            
            word = Word(rawWord, self.id)
            
            self.words.append(word)
            
            if word.dom == '_root':
                self.rootWord = word
                
            if word.dom in self.wordMap.keys():
                self.wordMap[word.dom].append(word)
            else:
                self.wordMap[word.dom] = [word]

        for word in self.words:
            if word.id in self.wordMap.keys():
                for d in self.wordMap[word.id]:
                    word.addWord(d)
        
        
class Word:
    def __init__(self, w: Element, sentId):
        self.dom = w.getAttribute('DOM')
        self.feat = w.getAttribute('FEAT')
        self.feat = self.feat.replace(' МЕТА', '').replace(' НАСТ ', ' ').replace(' СЛ', '').replace('COM', '').split()
        if len(self.feat) == 0:
            self.feat = ['S']
        self.id = w.getAttribute('ID')
        self.lemma = w.getAttribute('LEMMA')
        self.link = w.getAttribute('LINK')
        
        self.v = " ".join(t.nodeValue for t in w.childNodes if t.nodeType == t.TEXT_NODE).lower().strip()
        
        
        
        self.sentId = sentId
        
        self.connectedWords = []
        
    def addWord(self, w):
        self.connectedWords.append(w)
          

def parseSentences(xml_file):
    doc = parse(xml_file)
    sentences = doc.getElementsByTagName('S')
    res = []
    for sent in sentences:
        res.append(Sentence(sent))
    return res

from collections import deque

def find_word_depth(root, target_word):
    queue = deque([(root, 0)])
    
    while queue:
        current_word, depth = queue.popleft()
        
        if current_word.id == target_word.id:
            return depth
        
        for child in current_word.connectedWords:
            queue.append((child, depth+1))
    
    return None

def find_word_position(root, target_word):
    queue = deque([(root, 1)])

    current_tier = 1
    position_in_tier = 0
    
    tier_positions = {}

    while queue:
        current_node, tier = queue.popleft()
        
        if tier > current_tier:
            current_tier = tier
            position_in_tier = 0

        position_in_tier += 1
        
        tier_positions[current_node.id] = position_in_tier
        
        if current_node.id == target_word.id:
            return tier_positions[target_word.id]
        
        for child in current_node.connectedWords:
            queue.append((child, tier + 1))
    
    return None

i = 0

def get_pair(w):
    gramms = ''.join(sorted(w.feat))
    return w.v + gramms
    

def generate_pairs_json(sentences, sentence_id = None):
    res = {}
    id = 0
    sent = sentences[sentence_id - 1]
    s = sentences if sentence_id == None else [sent]
    for sent in s:
        for w in sent.words:
            key = get_pair(w)
            if key not in res:
                res[key] = {
                    'id': id,
                    'word': w.v,
                    'gramm': w.feat
                }
                id += 1
    return res 

def add_to_dict(d, key, value):
    if key in d:
        d[key].append(value)
    else:
        d[key] = [value]

def generate_json(sentences, pairs, sentence_id = None):
    sent = sentences[sentence_id - 1]
    s = sentences if sentence_id == None else [sent]
    for sent in tqdm(s):
        root = sent.wordMap['_root'][0]
        for w in sent.words:
            key = get_pair(w)
            m = pairs[key]
            
            add_to_dict(m, "n_tree", int(sent.id))
            add_to_dict(m, "n_depth", find_word_depth(root, w))
            add_to_dict(m, "n_pos", find_word_position(root, w))
                 
            if "connected_words" in m:
                connected_words = m["connected_words"]
            else: 
                connected_words = []
                
            for child in w.connectedWords:
                m_connected = {}
                f = True
                for mm in connected_words:
                    if mm["SSR"] == child.link:
                        key_child = get_pair(child)
                        mm["id"].append(pairs[key_child]["id"])
                        mm["id"] = list(set(mm["id"]))
                        f = False
                if f:
                    m_connected["SSR"] = child.link
                    
                    key_child = get_pair(child)
                    m_connected["id"] = [pairs[key_child]["id"]]
                    
                    connected_words.append(m_connected)
                    
            m["connected_words"] = connected_words
                
            pairs[key] = m
    return list(pairs.values())
            

#corpus_names = ['Tselina', 'sirija', 'pedagogika']
sentence_id = 6
corpus_names = ['sirija']
for corpus in corpus_names:
    
    xml_file = f'2016/{corpus}.xml'
    sentences = parseSentences(xml_file)
    pairs = generate_pairs_json(sentences, sentence_id = sentence_id)
    with open(f'magistr/{corpus}_word_pairs_{sentence_id}.json', 'w', encoding='utf-8') as f:
        json.dump(pairs, f, ensure_ascii = False, indent = 4)
        
    d = generate_json(sentences, pairs, sentence_id = sentence_id)
    
    with open(f'magistr/{corpus}_word_info_{sentence_id}.json', 'w', encoding='utf-8') as f:
        json.dump(d, f, ensure_ascii = False, indent = 4)
    #with open(f'magistr/{corpus}_word_ids.json', 'w', encoding='utf-8') as f:
    #    json.dump(ids, f, ensure_ascii = False, indent = 4)

