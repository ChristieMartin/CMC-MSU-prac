# Special Ints
Ordered set of integers made using massive with extendable length

```void AddSize()``` is used to extend size of massive if there is not enough memory for a new integer

```SpecialInts& operator,(int num)``` is used to add numbers conveniently, if there is not enough memory for a new number ```AddSize``` is used

```int Len()``` returns current amount of integers in massive

```int& operator[](int index)``` is used to index
