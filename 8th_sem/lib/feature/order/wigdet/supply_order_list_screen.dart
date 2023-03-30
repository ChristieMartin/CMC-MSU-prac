import 'package:flutter/material.dart';
import 'package:package_storage/modeling/order/supply_order.dart';
import 'package:package_storage/widgets/widgets.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class SupplyOrderListScreen extends StatelessWidget {
  const SupplyOrderListScreen({
    required this.orders,
    super.key,
  });
  final List<SupplyOrder> orders;

  @override
  Widget build(BuildContext context) {
    if (orders.isNotEmpty) {
      orders.sort((e1, e2) => e1.status.compareTo(e2.status));
    }
    return Scaffold(
      appBar: AppBar(
        title: Text(
          'Список поставок',
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
      body: ListView(
        children: orders.map((e) => _SupplyOrder(e)).toList(),
      ),
    );
  }
}

class _SupplyOrder extends StatelessWidget {
  const _SupplyOrder(this.order);
  final SupplyOrder order;

  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.symmetric(horizontal: 4.w, vertical: 1.h),
      padding: EdgeInsets.symmetric(horizontal: 2.w, vertical: 1.h),
      decoration: BoxDecoration(
        borderRadius: BorderRadius.circular(2.h),
        color: AppColors.green.withOpacity(0.4),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              Text(
                order.supplier,
                style: TextStyle(
                  fontSize: 18.5.sp,
                  color: AppColors.black,
                  fontWeight: FontWeight.w500,
                ),
              ),
              Text(
                ' - ${order.status}',
                style: TextStyle(
                  fontSize: 18.5.sp,
                  color: AppColors.black,
                ),
              ),
            ],
          ),
          const AppDivider(),
          Container(
            margin: EdgeInsets.symmetric(horizontal: 4.w, vertical: 1.h),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  'Дата отправки: ${order.orderingDay}',
                  style: TextStyle(
                    fontSize: 18.5.sp,
                    color: AppColors.black,
                  ),
                ),
                SizedBox(
                  height: 1.h,
                ),
                Text(
                  'Дата поставки: ${order.supplyDay}',
                  style: TextStyle(
                    fontSize: 18.5.sp,
                    color: AppColors.black,
                  ),
                ),
                Container(
                  margin: EdgeInsets.symmetric(horizontal: 2.w, vertical: 1.h),
                  padding:
                      EdgeInsets.symmetric(horizontal: 2.w, vertical: 0.5.h),
                  decoration: BoxDecoration(
                    borderRadius: BorderRadius.circular(2.h),
                    color: AppColors.green.withOpacity(0.4),
                  ),
                  child: OrderInfoWidget(order.orderInfo),
                )
              ],
            ),
          ),
        ],
      ),
    );
  }
}
