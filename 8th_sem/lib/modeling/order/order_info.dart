import '../product/product.dart';

class OrderInfo {
  final Product product;
  final int quantity;

  OrderInfo({
    required this.product,
    required this.quantity,
  });
}
