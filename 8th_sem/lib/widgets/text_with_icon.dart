import 'package:flutter/material.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class TextWithIcon extends StatelessWidget {
  const TextWithIcon({
    super.key,
    required this.icon,
    required this.leftText,
    required this.rightText,
  });
  final IconData icon;
  final String leftText;
  final String rightText;

  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.symmetric(vertical: 1.h),
      child: Row(
        children: [
          Container(
            decoration: BoxDecoration(
              color: AppColors.green,
              borderRadius: BorderRadius.circular(1.h),
            ),
            padding: EdgeInsets.all(2.w),
            child: Icon(
              icon,
              color: AppColors.white,
              size: 3.h,
            ),
          ),
          SizedBox(
            width: 4.w,
          ),
          SizedBox(
            width: 60.w,
            child: Text(
              leftText,
              style: TextStyle(
                fontSize: 18.5.sp,
                color: AppColors.black,
              ),
            ),
          ),
          Expanded(
            child: Text(
              rightText,
              textAlign: TextAlign.right,
              maxLines: 1,
              style: TextStyle(
                fontSize: 18.5.sp,
                color: AppColors.black,
                fontWeight: FontWeight.w500,
              ),
            ),
          ),
        ],
      ),
    );
  }
}
