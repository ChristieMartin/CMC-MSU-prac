import 'package:flutter/material.dart';
import 'package:package_storage/modeling/order/order_info.dart';
import 'package:package_storage/widgets/ui_kit/ui_kit.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class OrderInfoWidget extends StatelessWidget {
  const OrderInfoWidget(this.orderInfo, {super.key});
  final OrderInfo orderInfo;

  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.symmetric(vertical: 0.5.h),
      width: double.infinity,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Продукт: ${orderInfo.product.name}',
            style: TextStyle(
              fontSize: 18.5.sp,
              color: AppColors.black,
            ),
          ),
          Text(
            'Количество: ${orderInfo.quantity}',
            style: TextStyle(
              fontSize: 18.5.sp,
              color: AppColors.black,
            ),
          ),
        ],
      ),
    );
  }
}