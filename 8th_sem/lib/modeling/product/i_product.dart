class IProduct {
  final String name;
  int expiration;
  final int weight;
  double price;

  IProduct({
    required this.name,
    required this.expiration,
    required this.weight,
    required this.price,
  });

  factory IProduct.fromJson(Map<String, dynamic> json) => IProduct(
        name: json['name'] ?? '',
        expiration: json['expiration'] ?? 0,
        weight: json['weight'] ?? 0,
        price: (json['price'] ?? 0).toDouble(),
      );

  IProduct copyWith({
    int? expiration,
    double? price,
  }) =>
      IProduct(
        name: name,
        expiration: expiration ?? this.expiration,
        weight: weight,
        price: price ?? this.price,
      );

  @override
  bool operator ==(other) =>
      other is IProduct &&
      name == other.name &&
      weight == other.weight &&
      expiration == other.expiration;

  @override
  int get hashCode => Object.hash(name, weight, expiration);
}
