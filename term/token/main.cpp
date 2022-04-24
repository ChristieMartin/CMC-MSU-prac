#include <iostream>
#include <string>
#include <clocale>
#include <locale>

#include "tokenization.h"

using namespace std;

int main() {
    setlocale(LC_ALL, "ru_RU.UTF-8");
    locale imbue(const locale& L);
    wcout.imbue(locale("ru_RU.UTF-8"));

    std::wstring str = L"В таблице 3 приводятся результаты наших экспериментов. В первом эксперименте рассматриваются только контексты, содержащие терминальные знаки препинания. Мы выделили их в отдельную группу, поскольку большинство предложений заканчиваются именно одним из терминальных знаков. Кроме того, многие алгоритмы, описанные в литературе (см., например, [7]), не ставят своей целью анализ других знаков препинания. Во втором эксперименте рассматриваются все контексты.";

    Tokenization tokenization(str);

    std::wcout << L"Изначальный текст: " + tokenization.getText() << std::endl;
    std::wcout << L"Количество предложений: " << tokenization.getNumberOfSentence() << std::endl;
    std::wcout << L"Предложения:" << std::endl;
    for (int i = 1; i <= tokenization.getNumberOfSentence(); ++i) {
        std::wcout << L"------" << i << L"------" << std::endl << tokenization.getSentence(i).outputFormat()  << std::endl;
    }


    return 0;
}
