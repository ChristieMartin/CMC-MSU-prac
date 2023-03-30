import 'package:flutter/material.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class AppDivider extends StatelessWidget {
  const AppDivider({super.key});

  @override
  Widget build(BuildContext context) {
    return Divider(
      color: AppColors.lightBrown,
      thickness: 0.3.h,
      height: 2.h,
    );
  }
}
