import 'dart:math';
import 'dart:convert';
import 'package:flutter/services.dart' show rootBundle;

import '../order/order_info.dart';
import '../product/i_product.dart';
import '../product/package.dart';
import '../product/product_info.dart';

class Generator {
  static List<IProduct> allProducts = [];

  static Random random = Random();

  static List<String> suppliers = [
    'Четверочка',
    'Вкусхилл',
    'Магнат',
  ];

  static List<String> salePoints = [
    'Рынок',
    'Двор',
    'ВМК',
    'ГЗ МГУ',
    'Палатка',
    'Базар',
    'Ларек',
    'Европа',
    'Америка'
  ];

  static int randomDayValuesMin = 1;
  static int randomDayValuesMax = 1;

  static int randomQuantityMin = 1;
  static int randomQuantityMax = 10;

  static int randomSupplyQuantityMin = 1;
  static int randomSupplyQuantityMax = 3;

  // случайный поставщик
  static String getRandomSupplier() =>
      suppliers[random.nextInt(suppliers.length)];

  // случайная торговая точка
  static String getRandomSalePoint() =>
      salePoints[random.nextInt(salePoints.length)];

  static int getRandomDay() =>
      random.nextInt(randomDayValuesMax) + randomDayValuesMin; // от 1 до 3 дней

  static int getRandomQuantity() =>
      random.nextInt(randomQuantityMax) + randomQuantityMin; // от 1 до 10 штук

  static List<OrderInfo> getRandomOrderInfos(
      List<IProduct> discountedProducts) {
    int amount = random.nextInt(randomSupplyQuantityMax) +
        randomSupplyQuantityMin; // от 1 до 3 товаров в заказе
    List<OrderInfo> res = [];
    List<IProduct> discProducts = discountedProducts;
    discProducts.shuffle();
    for (int i = 0; i < amount; i++) {
      IProduct randProduct;
      if (discProducts.isNotEmpty && random.nextDouble() <= 0.75) {
        randProduct = discProducts.first;
        discProducts.removeAt(0);
      } else {
        randProduct = allProducts[random.nextInt(allProducts.length)];
      }

      // беру случайный товар из списка всех товаров
      res.add(
        OrderInfo(
          product: randProduct,
          quantity:
              getRandomQuantity(), // случайное количество случайного товара
        ),
      );
    }
    return res;
  }

  static List<Package> getInitPackages() {
    // упаковки, которые подгружаются в начале моделирования
    List<Package> packages = [];
    for (IProduct product in allProducts) {
      packages.add(
        Package(
          product: product,
          quantity: getRandomQuantity(),
          supplyDate: 0,
        ),
      );
    }
    return packages;
  }

  static List<ProductInfo> getInitProductInfos() {
    // информация о товарах которая подгружается в начале моделирования
    List<ProductInfo> productsInfos = [];
    for (IProduct product in allProducts) {
      productsInfos.add(
        ProductInfo(
          product: product,
          minQuantity:
              getRandomQuantity(), // случайное минимальное и максимальное количество
          maxQuantity: getRandomQuantity() + 10,
        ),
      );
    }
    return productsInfos;
  }

  static void setProductTypesAmount(int number) {
    // установляется количество типов продуктов
    allProducts = allProducts.take(number).toList();
  }

  static void setNumberOfSalePoints(int number) {
    // установляется количество торговых точек
    salePoints = salePoints.take(number).toList();
  }

  static Future<void> loadProductsFromJson() async {
    //тут будет подгрузка продуктов из json файла
    var data = await rootBundle.loadString('assets/data/products.json');
    List<IProduct> products = [];
    for (Map<String, dynamic> json in json.decode(data)) {
      products.add(IProduct.fromJson(json));
    }
    allProducts.addAll(products);
  }
}
