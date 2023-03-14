import '../product/i_product.dart';
import '../random/generator.dart';
import 'order.dart';
import 'order_info.dart';

class SupplyOrder extends Order {
  final OrderInfo orderInfo;
  final String supplier;
  final int supplyDay;

  SupplyOrder({
    required this.orderInfo,
    required this.supplier,
    required this.supplyDay,
    required super.status,
    required super.orderingDay,
  });

  // случайный заказ на поставку
  factory SupplyOrder.randomSupplyOrder(
    IProduct product,
    int currentDay,
  ) =>
      SupplyOrder(
        orderInfo: OrderInfo(
          product: product,
          quantity: Generator.getRandomQuantity(),
        ),
        supplier: Generator.getRandomSupplier(),
        supplyDay: Generator.getRandomDay(),
        orderingDay: currentDay,
        status: OrderStatus.pending,
      );
}
