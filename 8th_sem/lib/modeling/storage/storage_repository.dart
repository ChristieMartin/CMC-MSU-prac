import '../order/delivery_order.dart';
import '../order/order.dart';
import '../order/order_info.dart';
import '../order/supply_order.dart';
import '../product/product.dart';
import '../product/package.dart';
import '../random/generator.dart';
import 'storage.dart';

class StorageRepository {
  double moneyAmount;
  double income;
  double expenses;
  final Storage storage;

  List<SupplyOrder> supplyOrders;
  List<DeliveryOrder> deliveryOrders;

  List<DeliveryOrder> declinedDeliveryOrders;

  List<Package> sendingPackagesTomorrow;

  StorageRepository({
    this.moneyAmount = 0,
    this.income = 0,
    this.expenses = 0,
    this.supplyOrders = const [],
    this.deliveryOrders = const [],
    this.sendingPackagesTomorrow = const [],
    this.declinedDeliveryOrders = const [],
    required this.storage,
  });

  int get currentDay => storage.currentDay;

  void addProductList(List<Product> products, int supplyDate) {
    for (Product product in products) {
      addProduct(product, Generator.getRandomQuantity(), supplyDate);
    }
  }

  void addProduct(Product product, int quantity, int supplyDate) {
    if (quantity == 0) return;
    Package package;
    bool ex = false;
    if (quantity > 10) {
      package = Package(product: product, quantity: 10, supplyDate: supplyDate);
    } else if (quantity > 5) {
      package = Package(product: product, quantity: 5, supplyDate: supplyDate);
    } else {
      package =
          Package(product: product, quantity: quantity, supplyDate: supplyDate);
      ex = true;
    }
    storage.packages.add(package);
    if (ex) {
      return;
    }
    addProduct(product, quantity - package.quantity, supplyDate);
  }

  void nextDay() {
    // когда начинается новый день
    storage.removeExpired();

    storage.nextDay();

    createNewSupplyOrders();
    if (supplyOrders.isNotEmpty) {
      // обработка текущих заказов на поставку(которые в пути, или были открыты вчера)
      manageSupplyOrders();
    }
    // создание новых заказов на поставку

    sendingPackagesTomorrow = [];

    createNewDeliveryOrder();
    if (deliveryOrders.isNotEmpty) {
      // обработка текущих заказов на доставку(которые в пути, или были открыты вчера)
      manageDeliveryOrders();
    }
    // создание новых заказов на доставку
  }

  void manageDeliveryOrders() {
    // удаление тех заказов, которые были выполнены вчера
    deliveryOrders.removeWhere((order) => order.status == OrderStatus.ready);
    deliveryOrders
        .removeWhere((order) => declinedDeliveryOrders.contains(order));

    for (DeliveryOrder order in deliveryOrders) {
      if (order.deliveryDay - currentDay == 0) {
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
    order.status = OrderStatus.ready;
    Generator.occupiedSalePoints.remove(order.salePoint);
  }

  void manageDeliveryOrder(DeliveryOrder order) {
    if (Generator.occupiedSalePoints.contains(order.salePoint)) {
      declinedDeliveryOrders = [...declinedDeliveryOrders, order];
    } else {
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

          moneyAmount += info.quantity *
              info.product.weight *
              info.product.price *
              package.discount;
          income += info.quantity *
              info.product.weight *
              info.product.price *
              package.discount;
          // удаляю упаковку, которая будет отправлена со склада

          if (sum - window > info.quantity) {
            break;
          }
        }
        // упаковки, которые будут отправлены на следующий день
        sendingPackagesTomorrow = [
          ...sendingPackagesTomorrow,
          ...toSendPackages
        ];
      }
      Generator.occupiedSalePoints.add(order.salePoint);
    }
  }

  void createNewDeliveryOrder() {
    // создание нового заказа на доставку с вероятностью 50%
    if (Generator.random.nextBool()) {
      DeliveryOrder order = DeliveryOrder.randomDeliveryOrder(
        currentDay,
        storage.discountedPackages.map((e) => e.product).toList(),
      );
      deliveryOrders = [...deliveryOrders, order];
    }
  }

  void manageSupplyOrders() {
    // удаление заказов, которые были обработаны и показаны вчера
    supplyOrders.removeWhere((order) => order.status == OrderStatus.ready);

    for (SupplyOrder order in supplyOrders) {
      if (order.supplyDay - currentDay == 0) {
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
    Product suppliedProduct = order.orderInfo.product.copyWith(
        expiration: order.supplyDay + order.orderInfo.product.expiration);
    addProduct(suppliedProduct, order.orderInfo.quantity, order.supplyDay);

    order.status = OrderStatus.ready;
  }

  void createNewSupplyOrders() {
    List<Product> needsDelivery = storage.needsSupplyProducts;
    List<SupplyOrder> res = [];
    double orderPrice;
    for (Product p in needsDelivery) {
      SupplyOrder order = SupplyOrder.randomSupplyOrder(p, currentDay);
      orderPrice = order.orderInfo.quantity *
          order.orderInfo.product.weight *
          order.orderInfo.product.price;
      if (moneyAmount - orderPrice < 0) break;
      moneyAmount -= orderPrice;
      expenses += orderPrice;
      res.add(order);
    }
    supplyOrders = [...supplyOrders, ...res];
  }
}
