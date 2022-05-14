#include "tokenization.h"
#include <iostream>
#include <sstream>
#include <fstream>
#include <codecvt>


Tokenization::Tokenization(const std::wstring& source, const std::string& pathToAbbreviationBase)
    : text(source)
    , length(source.length())
    , numberOfSentence(0)
    , sentences(std::vector<Sentence>())
    , threshold(3)
    , correct(0)
    , mistaken(0)
    , skipped(0)
    , original(0)
{
    // Заполняем базу сокращений из файла
    std::ifstream fin(pathToAbbreviationBase);
    if (fin.is_open()) {
        while (!fin.eof()) {
            std::string input;
            fin >> input;

            std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
            std::wstring word = converter.from_bytes(input);
            if (word != L"") {
                abbreviationDatabase[word] = threshold;
            }
        }
    }
    fin.close();

    textParsing();

    // Добавляем новые сокращения в файл с базой сокращений
    std::ofstream fout;
    fout.open(pathToAbbreviationBase);
    if (fout.is_open()) {
        std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
        for (auto [key, repeats] : abbreviationDatabase) {
            if (repeats >= threshold && key != L"") {
                std::string word = converter.to_bytes(key);
                fout << word << "\n";
            }
        }
    }
    fout.close();
}

void Tokenization::textParsing() { // Считываем текст и делим его на предложения
    int begin = 0;
    bool sentenceEnded = true;

    bool parsingWord = false;
    int numberOfWords = 0;

    for (int i = 0; i < length; ++i) {

        // Считаем слова
        if ((iswalpha(text[i])) || iswdigit(text[i])) {
            if (!parsingWord) {
                parsingWord = true;
            }
        } else if (parsingWord) {
            numberOfWords++;
            parsingWord = false;
        }

        // Обработка предложений
        if (sentenceEnded && (!isspace(text[i]))) {
            begin = i;
            sentenceEnded = false;
        } else if (isEndOfSentence(i, numberOfWords) && !sentenceEnded) {
            numberOfSentence++;
            sentences.emplace_back(Sentence(&text, begin, i));
            sentenceEnded = true;

            // обнуляем счетчик слов
            numberOfWords = 0;
            parsingWord = false;

            if (i < length && text[i + 1] == '\n') { // проверка на корректность конца предложения при размеченом тексте
                correct++;
            } else {
                mistaken++;
                mistakenEndsOfSentences.push_back(numberOfSentence - 1);
            }
        }

        if (text[i] == L'\n') { // число настоящих границ предложения
            original++;
            if (!sentenceEnded)
                skipped++;
                skippedEndsOfSentences.push_back(numberOfSentence);
        }
    }

}

bool Tokenization::isEndOfSentence(int i, int numberOfWords) { // Является ли точка окончанием предложения
    if (text[i] == '!')
        return true;
    if (text[i] == '?') {
        std::wstring next = nextPseudoWord(i);

        if (next == L"!") { // Случай "?!"
            return false;
        }
        return true;
    }
    if (text[i] == '.') {
        std::wstring prev = previousPseudoWord(i); // Псевдослово до точки (псевдослово - набор символов без пробелов)
        std::wstring abbreviationPrev = previousWordForAbbreviation(i); // Возможное сокращение

        std::wstring next = nextPseudoWord(i);     // Псевдослово после точки
        std::wstring realNext = nextRealWord(i);   // "Настоящее" слово после точки (псевдослово, включающее хотя бы одну букву или цифру)

        if (i + 1 < length && !iswspace(text[i + 1])) { // Точка - часть слова (напр. веб-адрес)
            return false;
        }
        if (next[0] == '.') { // проверка на многоточие
            return false;
        }
        if (next == L"" || realNext == L"") { // последнее предложение, нет следующего слова
            return true;
        }
        if (iswdigit(prev[0]) && iswdigit(text[i + 1])) { // плавающая точка числа
            return false;
        }
        if (numberOfWords == 1 && isNumber(prev)) { // проверка на нумерованный список (1. 2. 3. ...)
            return false;
        }
        if (!iswupper(realNext[0]) && !iswdigit(realNext[0])) { // если следующее слово начинается не с большой буквы - не конец предложения
            abbreviationDatabase[abbreviationPrev]++; // в таком случае добавляем в базу сокращений
            return false;
        }
        if (abbreviationPrev != L"" && abbreviationDatabase.count(abbreviationPrev) && abbreviationDatabase[abbreviationPrev] >= threshold) { // сравниваем слово до точки со словами из базы сокращений
            return false;
        }

        return true;
    }
    return false;
}

std::wstring Tokenization::previousPseudoWord(int i) {
    int begin = i - 1;
    while (begin >= 0 && !isspace(text[begin])) {
        begin--;
    }
    return std::wstring(text, begin + 1, i - begin - 1);
}

std::wstring Tokenization::previousWordForAbbreviation(int i) {
    int begin = i - 1;
    while (begin >= 0 && (iswalpha(text[begin]) || iswdigit(text[begin]))) {
        begin--;
    }
    std::wstring str = std::wstring(text, begin + 1, i - begin - 1);
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    return str;
}

std::wstring Tokenization::nextPseudoWord(int i) {
    int begin = i + 1;
    while (begin < length && isspace(text[begin])) {
        begin++;
    }
    if (begin == length) {
        return L"";
    }
    int end = begin;
    while (end < length && !isspace(text[end])) {
        end++;
    }
    return std::wstring(text, begin, end - begin);
}

std::wstring Tokenization::nextRealWord(int i) {
    int begin = i + 1;
    while (begin < length && !iswalpha(text[begin]) && !iswdigit(text[begin])) {
        begin++;
    }
    if (begin == length) {
        return L"";
    }
    int end = begin;
    while (end < length && !isspace(text[end])) {
        end++;
    }
    return std::wstring(text, begin, end - begin);
}





void Sentence::sentenceParsing() { // Разделение предложения на слова и вставные предложения
    bool parsingWord = false;
    bool quotations = false;
    bool parenthesis = false;
    bool dash = false;

    int beginWord = 0;
    int beginInsertedSentence = 0;

    for (int i = begin; i < end; ++i) { // Проходим по всему предложению
        wchar_t c = text->at(i);
        if (iswspace(c)) { // Если встретили пробел - записываем считанное слово
            if (parsingWord) {
                words.emplace_back(Borders(beginWord, i - 1));
                parsingWord = false;
            }
        }
        if ((iswalpha(c)) || iswdigit(c)) { // проверяем что символ буква или цифра, тогда часть слова
            if (!parsingWord) {
                beginWord = i;
                parsingWord = true;
            }
        } else if (c == L'-') { // дефис (часть слова)
            if (parsingWord) {
                continue;
            } else if (iswdigit(text->at(i + 1))) { // дефис как минус при записи числа
                beginWord = i;
                parsingWord = true;
            }
        } else {
            if (parsingWord) {
                words.emplace_back(Borders(beginWord, i - 1));
                parsingWord = false;
            }
        }


        if (c == L'–') { // тире начинает и заканчивает вставное предложение
            if (dash) {
                insertedSentences.emplace_back(Borders(beginInsertedSentence, i));
                dash = false;
            } else if (!quotations && !parenthesis) {
                beginInsertedSentence = i;
                dash = true;
            }
        }
        if (c == L'"') { // кавычки начинает и заканчивает вставное предложение
            if (quotations) {
                insertedSentences.emplace_back(Borders(beginInsertedSentence, i));
                quotations = false;
            } else if (!dash && !parenthesis){
                beginInsertedSentence = i;
                quotations = true;
            }
        }
        if (c == L'(') {  // скобки начинает и заканчивает вставное предложение
            if (!dash && !quotations && !parenthesis){
                beginInsertedSentence = i;
                parenthesis = true;
            }
        }
        if (c == L')') {
            if (parenthesis) {
                insertedSentences.emplace_back(Borders(beginInsertedSentence, i));
                parenthesis = false;
            }
        }
    }
    if (parsingWord) {
        words.emplace_back(Borders(beginWord, end - 1));
    }
    if (dash || quotations || parenthesis) {
        insertedSentences.emplace_back(Borders(beginInsertedSentence, end - 1));
    }
}

std::wstring Sentence::outputFormat() { // Удобный вывод информации о предложении для отладки
    std::wstringstream res;
    res << L"Предложение: " << getSentence() << L'\n';
    res << L"Кол-во слов: " << getNumberOfWords() << L"; Кол-во вставных предложений: " << getNumberOfInsertedSentences() << L'\n';
    if (getNumberOfInsertedSentences() > 0)
        res << L"Вставные предложения: \n";
    for (int i = 0; i < getNumberOfInsertedSentences(); ++i) {
        res << getInsertedSentence(i + 1) + L'\n';
    }
    if (getNumberOfWords() > 0)
        res << L"Слова: \n";
    for (int i = 0; i < getNumberOfWords(); ++i) {
        res << getWord(i + 1) + L'\n';
    }
    return res.str();
}

bool Tokenization::isNumber(const std::wstring& str) {
    for (wchar_t const &ch : str) {
        if (std::isdigit(ch) == 0)
            return false;
    }
    return true;
}
