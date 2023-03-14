class IProduct {
  final String name;
  final int expiration;
  final int weight;
  final double price;

  const IProduct({
    required this.name,
    required this.expiration,
    required this.weight,
    required this.price,
  });

  set weight(int newWeight) {
    if (newWeight < 0) {
      weight = 0;
    } else {
      weight = newWeight;
    }
  }

  set price(double newPrice) {
    if (newPrice < 0) {
      price = 0;
    } else {
      price = newPrice;
    }
  }

  set expiration(int newExpiration) {
    if (newExpiration < 0) {
      expiration = 0;
    } else {
      expiration = newExpiration;
    }
  }

  factory IProduct.fromJson(Map<String, dynamic> json) => IProduct(
        name: json['name'] ?? '',
        expiration: json['expiration'] ?? 0,
        weight: json['weight'] ?? 0,
        price: (json['price'] ?? 0).toDouble(),
      );
}
