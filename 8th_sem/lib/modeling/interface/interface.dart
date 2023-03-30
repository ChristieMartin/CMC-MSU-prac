import 'dart:math';

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

  double get moneyAmount => storageRepository.moneyAmount;

  List<IProduct> get currentProductsInStorage =>
      storageRepository.storage.allProducts;

  List<Package> get currentPackages => storageRepository.storage.packages;

  List<Package> get discountedPackages =>
      storageRepository.storage.discountedPackages;

  List<Package> get expiredPackages =>
      storageRepository.storage.expiredPackages;

  List<DeliveryOrder> get deliveryOrders => storageRepository.deliveryOrders;

  List<SupplyOrder> get supplyOrders => storageRepository.supplyOrders;

  List<Package> get sendingPackagesTomorrow =>
      storageRepository.sendingPackagesTomorrow;

  void nextDayButtonClicked() {
    if (currentDay != totalAmountOfDays) {
      for (int i = 0;
          i < min(totalAmountOfDays - currentDay, stepAmount);
          i++) {
        storageRepository.nextDay();
      }
    }
  }
}
