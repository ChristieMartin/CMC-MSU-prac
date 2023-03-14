import 'package:flutter/material.dart';
import 'package:package_storage/modeling/product/i_product.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class ProductScreen extends StatelessWidget {
  const ProductScreen({
    super.key,
    required this.product,
  });
  final IProduct product;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(
          product.name,
          style: TextStyle(
            color: AppColors.white,
            fontSize: 20.sp,
            fontWeight: FontWeight.w500,
          ),
        ),
        leading: IconButton(
          onPressed: () {
            Navigator.pop(context);
          },
          icon: const Icon(
            Icons.arrow_back_ios,
            color: AppColors.white,
          ),
        ),
      ),
      body: Container(
        padding: EdgeInsets.symmetric(horizontal: 4.w, vertical: 1.h),
        child: SingleChildScrollView(
          physics: const BouncingScrollPhysics(),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              _TextWithIcon(
                icon: Icons.schedule,
                leftText: 'Срок годности',
                rightText: product.expiration.toString(),
              ),
              _TextWithIcon(
                icon: Icons.monetization_on_rounded,
                leftText: 'Цена',
                rightText: product.price.toStringAsFixed(2),
              ),
              _TextWithIcon(
                icon: Icons.scale_rounded,
                leftText: 'Вес',
                rightText: product.expiration.toString(),
              ),
            ],
          ),
        ),
      ),
    );
  }
}

class _TextWithIcon extends StatelessWidget {
  const _TextWithIcon({
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
          Text(
            '$leftText - ',
            style: TextStyle(
              fontSize: 18.5.sp,
              color: AppColors.black,
            ),
          ),
          Text(
            rightText,
            style: TextStyle(
              fontSize: 18.5.sp,
              color: AppColors.black,
              fontWeight: FontWeight.w500,
            ),
          )
        ],
      ),
    );
  }
}
