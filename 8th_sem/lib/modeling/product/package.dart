import 'i_product.dart';

class Package {
  final IProduct product;
  final int quantity;
  final int supplyDate;

  const Package({
    required this.product,
    required this.quantity,
    required this.supplyDate,
  });
}
