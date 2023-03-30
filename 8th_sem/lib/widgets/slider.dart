import 'package:flutter/material.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';

class AppSlider extends StatelessWidget {
  const AppSlider({
    required this.min,
    required this.max,
    required this.value,
    required this.onChanged,
    super.key,
  });
  final double min;
  final double max;
  final double value;
  final void Function(double) onChanged;

  @override
  Widget build(BuildContext context) {
    return Slider(
      inactiveColor: AppColors.lightBrown.withOpacity(0.5),
      activeColor: AppColors.lightBrown,
      min: min,
      max: max,
      value: value,
      thumbColor: AppColors.green,
      onChanged: onChanged,
    );
  }
}
