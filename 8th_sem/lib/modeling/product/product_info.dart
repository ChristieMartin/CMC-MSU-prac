import 'i_product.dart';

class ProductInfo {
  final IProduct product;
  final int minQuantity;
  final int maxQuantity;

  const ProductInfo({
    required this.product,
    required this.minQuantity,
    required this.maxQuantity,
  });
}
