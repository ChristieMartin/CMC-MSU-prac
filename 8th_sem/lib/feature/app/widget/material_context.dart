import 'package:flutter/material.dart';
import 'package:package_storage/feature/home/widget/home_screen.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';

class MaterialContext extends StatelessWidget {
  const MaterialContext({
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      debugShowCheckedModeBanner: false,
      home: const HomeScreen(),
      theme: AppTheme.appTheme,
    );
  }
}
