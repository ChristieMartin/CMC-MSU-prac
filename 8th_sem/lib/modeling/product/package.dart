import 'i_product.dart';

class Package {
  final IProduct product;
  final int quantity;
  final int supplyDate;
  double discount;

  Package({
    required this.product,
    required this.quantity,
    required this.supplyDate,
    this.discount = 1,
  });

  @override
  bool operator ==(other) =>
      other is Package &&
      product == other.product &&
      quantity == other.quantity &&
      supplyDate == other.supplyDate;

  @override
  int get hashCode => Object.hash(product, quantity, supplyDate);
}
