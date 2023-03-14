import 'package:flutter/material.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';

class AppTheme {
  static ThemeData appTheme = ThemeData(
    scaffoldBackgroundColor: AppColors.white,
    appBarTheme: const AppBarTheme(
      backgroundColor: AppColors.green,
      centerTitle: true,
      elevation: 0,
    ),
    fontFamily: 'Rubik',
    
  );
}
