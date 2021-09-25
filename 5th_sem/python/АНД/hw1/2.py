# -*- coding: utf-8 -*-
cap_letters = set("АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")
punct = set(".!?")
sent = []
end = False
f = open("text.txt", "r", encoding = "utf-8").read()
temp = ""
j = 0
for i in f:
    if end and i in cap_letters:
        sent.append(temp)
        end = False
        temp = ""
    if end and i != " " and i not in cap_letters:
        end = False
    if i in punct:
        end = True
    if i != "\n":
        temp += i
if temp != "":
    sent.append(temp)
for p, n in enumerate(sent):
    print(p + 1, n)
    
    
