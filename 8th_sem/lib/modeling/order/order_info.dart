import '../product/i_product.dart';

class OrderInfo {
  final IProduct product;
  final int quantity;

  OrderInfo({
    required this.product,
    required this.quantity,
  });
}
