ss = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZабвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ "
f = open("text.txt", "r", encoding = "utf-8").read()
p = " ".join(f.split())
p = " " + p
print("Биграммы:")
for i in range(0, len(p) - 1):
    if p[i] in ss and p[i+1] in ss:
        print(p[i].lower(), p[i + 1].lower(), sep = "")
print("Триграммы:")
for i in range(0, len(p) - 2):
    if p[i] in ss and p[i+1] in ss and p[i+2] in ss:
        print(p[i].lower(), p[i + 1].lower(), p[i + 2].lower(), sep = "")
