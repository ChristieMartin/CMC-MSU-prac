abstract class Order {
  OrderStatus status;
  final int orderingDay;

  Order({
    required this.status,
    required this.orderingDay,
  });
}

enum OrderStatus {
  pending,
  shipping,
  ready;

  @override
  String toString() {
    switch (this) {
      case OrderStatus.pending:
        return "В обработке";
      case OrderStatus.shipping:
        return "В пути";
      case OrderStatus.ready:
        return "Выполнен";
    }
  }

  int compareTo(OrderStatus other) => index.compareTo(other.index);
}
