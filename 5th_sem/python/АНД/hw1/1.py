# -*- coding: utf-8 -*-
local_available = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'*+-/=?^_`{|}~.")
domain_available = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-.]")
spec = set("(),:;<>@[\] ")


def check_local(local):
    """
    1. прописные и строчные латинские буквы от A до Z и от a до z ;
    2. цифры от 0 до 9 ;
    3. специальные символы !#$%&'*+-/=?^_`{|}~ ;
    4. точка . , при условии, что это не первый или последний символ, если он не заключен в кавычки, а также при условии, что он не появляется последовательно, если он не заключен в кавычки
    5. пробел и символы "(),:;<>@[\] разрешены только внутри строки в кавычках
    """
    in_commas = False
    if local[0] == "." or local[-1] == ".":
        return False
    for c in local:
        if c == '\"':
            in_commas = not in_commas
            continue

        if not in_commas:
            if c == ".":
                if dot:
                    return False
                dot = True
            else:
                dot = False
            if not c in local_available or c in spec:
                return False
        else:
            if not c in local_available and not c in spec:
                return False
    return True


def check_domain(domain):
    """
    1. строчные латинские буквы: abcdefghijklmnopqrstuvwxyz ,
    2. заглавные латинские буквы: ABCDEFGHIJKLMNOPQRSTUVWXYZ ,
    3. цифры: 0123456789 ,
    4. дефис: - (не первый и не последний символ),
    5. точка: . (не первый и не последний символ и не две подряд),
    6. может содержать строку, заключенную в квадратные скобки
    """
    if domain[0] == "-" or domain[-1] == "-" or domain.count(".") == 0:
        return "no"
    if domain[0] == "." or domain[-1] == ".":
        return "no"
    if domain.count("..") != 0:
        return "no"
    if domain.count("]") > 1:
        return "no"
    if domain.count("[") != 0:
        if domain[0] != "[":
            return "no"
    for c in domain:
        if not c in domain_available:
            return "no"
    return "yes"


def check_email(st):
    # макс. 64@255 символов, всего не более 256
    if st.count("@") == 0 or len(st) > 256:
        return "no"
    local = st.split("@")[0]
    domain = st.split("@")[1]
    if len(local) == 0 or len(domain) == 0:
        return "no"
    if len(local) > 64 or len(domain) > 255:
        return "no"

    if check_local(local):
        return check_domain(domain)
    else:
        return "no"


s = input()
print(check_email(s))
