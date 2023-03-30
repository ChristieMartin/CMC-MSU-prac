import 'package:package_storage/modeling/random/generator.dart';

import '../product/i_product.dart';
import '../product/package.dart';
import '../product/product_info.dart';

class Storage {
  List<Package> packages;
  final List<ProductInfo> productsInfo;
  int currentDay;

  Storage({
    required this.packages,
    required this.productsInfo,
    this.currentDay = 0,
  });

  void nextDay() {
    currentDay += 1;
  }

  List<IProduct> get allProducts {
    List<IProduct> res = [];
    for (Package p in packages) {
      res.add(p.product);
    }
    return res;
  }

  List<Package> get discountedPackages {
    List<Package> res = [];
    for (Package p in packages) {
      if (p.product.expiration + p.supplyDate - currentDay <= 2) {
        // просроченный товар === дата поставки + дни до просрочки - текущий день = 1
        // то есть до дня просрочки остался один день
        if (p.discount == 1) {
          p.discount = 0.75;
        }

        res.add(p);
      }
    }
    return res;
  }

  List<Package> get expiredPackages {
    // просроченные товары
    List<Package> res = [];
    for (Package p in packages) {
      if (p.product.expiration - currentDay < 1) {
        // просроченный товар === дата поставки + дни до просрочки - текущий день < 1
        // то есть наступил или прошел день просрочки
        res.add(p);
      }
    }
    return res;
  }

  List<IProduct> get needsSupplyProducts {
    List<IProduct> res = expiredPackages.map((e) => e.product).toList();
    // все просроченные товары + товары, которых не хватает на складе
    for (ProductInfo p in productsInfo) {
      if (p.minQuantity > p.product.weight && !res.contains(p.product)) {
        res.add(p.product);
      }
    }
    return res;
  }

  void removeExpired() {
    print(expiredPackages.length);
    packages.removeWhere((element) => expiredPackages.contains(element));
  }
}
