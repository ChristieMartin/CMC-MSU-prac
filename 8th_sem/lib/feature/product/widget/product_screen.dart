import 'package:flutter/material.dart';
import 'package:package_storage/modeling/product/product.dart';
import 'package:package_storage/widgets/widgets.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class ProductScreen extends StatelessWidget {
  const ProductScreen({
    super.key,
    required this.product,
  });
  final Product product;

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
              TextWithIcon(
                icon: Icons.schedule,
                leftText: 'Срок годности',
                rightText: product.expiration.toString(),
              ),
              TextWithIcon(
                icon: Icons.monetization_on_rounded,
                leftText: 'Цена',
                rightText: product.price.toStringAsFixed(2),
              ),
              TextWithIcon(
                icon: Icons.scale_rounded,
                leftText: 'Вес',
                rightText: product.weight.toString(),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
