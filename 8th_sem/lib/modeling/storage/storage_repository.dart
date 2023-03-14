import '../order/delivery_order.dart';
import '../order/order.dart';
import '../order/order_info.dart';
import '../order/supply_order.dart';
import '../product/i_product.dart';
import '../product/package.dart';
import '../random/generator.dart';
import 'storage.dart';

class StorageRepository {
  final double moneyAmount;
  final Storage storage;

  final List<SupplyOrder> supplyOrders;
  final List<DeliveryOrder> deliveryOrders;

  final List<Package> sendingPackagesTomorrow;

  StorageRepository({
    this.moneyAmount = 0,
    this.supplyOrders = const [],
    this.deliveryOrders = const [],
    this.sendingPackagesTomorrow = const [],
    required this.storage,
  });

  set supplyOrders(List<SupplyOrder> newSupplyOrders) =>
      supplyOrders = newSupplyOrders;

  set deliveryOrders(List<DeliveryOrder> newDeliveryOrders) =>
      deliveryOrders = newDeliveryOrders;

  set sendingPackagesTomorrow(List<Package> newSendingPackagesTomorrow) =>
      sendingPackagesTomorrow = newSendingPackagesTomorrow;

  set moneyAmount(double newMoneyAmount) {
    if (moneyAmount < 0) {
      moneyAmount = 0;
    } else {
      moneyAmount = newMoneyAmount;
    }
  }

  int get currentDay => storage.currentDay;

  set currentDay(int newCurrentDay) {
    storage.currentDay = newCurrentDay;
  }

  void nextDay() {
    // когда начинается новый день
    currentDay += 1;

    if (supplyOrders.isNotEmpty) {
      // обработка текущих заказов на поставку(которые в пути, или были открыты вчера)
      manageSupplyOrders();
    }
    // создание новых заказов на поставку
    createNewSupplyOrders();

    sendingPackagesTomorrow = [];
    if (deliveryOrders.isNotEmpty) {
      // обработка текущих заказов на доставку(которые в пути, или были открыты вчера)
      manageDeliveryOrders();
    }
    // создание новых заказов на доставку
    createNewDeliveryOrder();
  }

  void manageDeliveryOrders() {
    // удаление тех заказов, которые были выполнены вчера
    deliveryOrders.removeWhere((order) => order.status == OrderStatus.ready);

    for (DeliveryOrder order in deliveryOrders) {
      if (order.orderingDay == order.deliveryDay) {
        // обработка выполненого заказа на доставку
        manageReadyDeliveryOrder(order);
        continue;
      }

      if (order.orderingDay == currentDay - 1) {
        // вчера заказ был получен, сегодня отправлен
        order.status = OrderStatus.shipping;
      }

      if (order.status == OrderStatus.pending) {
        // обработка заказа, ждущего обработку
        manageDeliveryOrder(order);
      }
    }
  }

  void manageReadyDeliveryOrder(DeliveryOrder order) {
    for (OrderInfo info in order.orderInfos) {
      // подсчет прибыли
      moneyAmount += info.quantity * info.product.price;
    }
    order.status = OrderStatus.ready;
  }

  void manageDeliveryOrder(DeliveryOrder order) {
    // распределение упаковок для отправки

    // окно при котором можно отправлять при меньшем количестве килограмм товара
    int window = 5;

    for (OrderInfo info in order.orderInfos) {
      List<Package> allPackages = storage.packages
          .where((element) => element.product == info.product)
          .toList();
      List<Package> toSendPackages = [];

      // для подсчета общего веса для этого товара
      int sum = 0;

      for (Package package in allPackages) {
        // сумма веса в упаковке на складе
        sum += package.quantity * package.product.weight;

        toSendPackages.add(package);
        storage.packages.remove(package);
        // удаляю упаковку, которая будет отправлена со склада

        if (sum - window > info.quantity) {
          break;
        }
      }
      // упаковки, которые будут отправлены на следующий день
      sendingPackagesTomorrow.addAll(toSendPackages);
    }
  }

  void createNewDeliveryOrder() {
    // создание нового заказа на доставку с вероятностью 50%
    if (Generator.random.nextBool()) {
      DeliveryOrder order = DeliveryOrder.randomDeliveryOrder(currentDay);
      deliveryOrders.add(order);
    }
  }

  void manageSupplyOrders() {
    // удаление заказов, которые были обработаны и показаны вчера
    supplyOrders.removeWhere((order) => order.status == OrderStatus.ready);

    for (SupplyOrder order in supplyOrders) {
      if (order.supplyDay == currentDay) {
        manageReadySupplyOrder(order);
        continue;
      }
      if (order.orderingDay == currentDay - 1) {
        // если заказ был вчера, то сегодня он уже в пути
        order.status = OrderStatus.shipping;
      }
    }
  }

  void manageReadySupplyOrder(SupplyOrder order) {
    // новые товары поступают на склад
    order.orderInfo.product.weight += order.orderInfo.quantity;
    order.orderInfo.product.expiration +=
        10; //? это тоже стоит рандомизировать?

    // подсчет убытков
    //? стоит ли это делать, когда заказ только оформляется?
    moneyAmount -= order.orderInfo.quantity * order.orderInfo.product.price;

    order.status = OrderStatus.ready;
  }

  void createNewSupplyOrders() {
    List<IProduct> needsDelivery = storage.needsSupplyProducts;
    List<SupplyOrder> res = [];
    for (IProduct p in needsDelivery) {
      SupplyOrder order = SupplyOrder.randomSupplyOrder(p, currentDay);
      res.add(order);
    }
    supplyOrders.addAll(res);
  }
}
