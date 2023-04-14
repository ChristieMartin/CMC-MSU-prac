class Product {
  final String name;
  int expiration;
  final int weight;
  double price;

  Product({
    required this.name,
    required this.expiration,
    required this.weight,
    required this.price,
  });

  factory Product.fromJson(Map<String, dynamic> json) => Product(
        name: json['name'] ?? '',
        expiration: json['expiration'] ?? 0,
        weight: json['weight'] ?? 0,
        price: (json['price'] ?? 0).toDouble(),
      );

  Product copyWith({
    int? expiration,
    double? price,
  }) =>
      Product(
        name: name,
        expiration: expiration ?? this.expiration,
        weight: weight,
        price: price ?? this.price,
      );

  @override
  bool operator ==(other) =>
      other is Product &&
      name == other.name &&
      weight == other.weight &&
      expiration == other.expiration;

  @override
  int get hashCode => Object.hash(name, weight, expiration);
}
