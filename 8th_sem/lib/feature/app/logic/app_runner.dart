import 'package:flutter/material.dart';
import 'package:package_storage/feature/app/widget/app.dart';
import 'package:package_storage/modeling/random/generator.dart';

class AppRunner {
  static Future<void> initializeAndStart() async {
    WidgetsFlutterBinding.ensureInitialized();

    await Generator.loadProductsFromJson();

    Paint.enableDithering = true;

    App.run();
  }
}
