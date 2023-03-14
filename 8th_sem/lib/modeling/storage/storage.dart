import '../product/i_product.dart';
import '../product/package.dart';
import '../product/product_info.dart';

class Storage {
  final List<Package> packages;
  final List<ProductInfo> productsInfo;
  final int currentDay;

  Storage({
    required this.packages,
    required this.productsInfo,
    this.currentDay = 0,
  });

  set currentDay(int newCurrentDay) => currentDay = newCurrentDay;

  set packages(List<Package> newPackages) => packages = newPackages;

  List<IProduct> get allProducts {
    List<IProduct> res = [];
    for (Package p in packages) {
      res.add(p.product);
    }
    return res;
  }

  List<IProduct> get discountedProducts {
    List<IProduct> res = [];
    for (Package p in packages) {
      if (p.product.expiration + p.supplyDate - currentDay == 1) {
        // просроченный товар === дата поставки + дни до просрочки - текущий день = 1
        // то есть до дня просрочки остался один день
        res.add(p.product);
      }
    }
    return res;
  }

  List<IProduct> get expiredProducts {
    // просроченные товары
    List<IProduct> res = [];
    for (Package p in packages) {
      if (p.product.expiration + p.supplyDate - currentDay < 1) {
        // просроченный товар === дата поставки + дни до просрочки - текущий день < 1
        // то есть наступил или прошел день просрочки
        res.add(p.product);
      }
    }
    return res;
  }

  List<IProduct> get needsSupplyProducts {
    List<IProduct> res = expiredProducts;
    // все просроченные товары + товары, которых не хватает на складе
    for (ProductInfo p in productsInfo) {
      if (p.minQuantity > p.product.weight && !res.contains(p.product)) {
        res.add(p.product);
      }
    }
    return res;
  }
}
