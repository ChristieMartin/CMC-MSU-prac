abstract class Order {
  final OrderStatus status;
  final int orderingDay;

  Order({
    required this.status,
    required this.orderingDay,
  });

  set status(OrderStatus newStatus) => status = newStatus;
}

enum OrderStatus { pending, shipping, ready }
