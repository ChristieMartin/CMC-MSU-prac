import '../order/delivery_order.dart';
import '../order/supply_order.dart';
import '../product/i_product.dart';
import '../product/package.dart';
import '../storage/storage_repository.dart';

class Interface {
  final int totalAmountOfDays;
  final int stepAmount;
  final StorageRepository storageRepository;

  Interface({
    required this.totalAmountOfDays,
    required this.stepAmount,
    required this.storageRepository,
  });

  int get currentDay => storageRepository.currentDay;

  List<IProduct> get currentProductsInStorage =>
      storageRepository.storage.allProducts;

  List<DeliveryOrder> get deliveryOrders => storageRepository.deliveryOrders;

  List<SupplyOrder> get supplyOrders => storageRepository.supplyOrders;

  List<Package> get sendingPackagesTomorrow =>
      storageRepository.sendingPackagesTomorrow;

  void nextDayButtonClicked() => storageRepository.nextDay();
}
