import '../random/generator.dart';
import 'order.dart';
import 'order_info.dart';

class DeliveryOrder extends Order {
  final List<OrderInfo> orderInfos;
  final String salePoint;
  final int deliveryDay;

  DeliveryOrder({
    required this.orderInfos,
    required this.salePoint,
    required this.deliveryDay,
    required super.status,
    required super.orderingDay,
  });

  factory DeliveryOrder.randomDeliveryOrder(int currentDay) => DeliveryOrder(
    salePoint: Generator.getRandomSalePoint(),
    orderingDay: currentDay,
    status: OrderStatus.pending,
    deliveryDay: Generator.getRandomDay(),
    orderInfos: Generator.getRandomOrderInfos(),
  );
}
