import 'package:flutter/material.dart';
import 'package:package_storage/feature/day/widget/drawer.dart';
import 'package:package_storage/modeling/interface/interface.dart';
import 'package:package_storage/widgets/widgets.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class DayScreen extends StatefulWidget {
  const DayScreen({
    required this.interface,
    Key? key,
  }) : super(key: key);
  final Interface interface;

  @override
  State<DayScreen> createState() => _DayScreenState();
}

class _DayScreenState extends State<DayScreen> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(
          '${widget.interface.currentDay} день',
          style: TextStyle(
            color: AppColors.white,
            fontSize: 20.sp,
            fontWeight: FontWeight.w500,
          ),
        ),
        leading: Builder(builder: (context) {
          return IconButton(
            onPressed: () {
              Scaffold.of(context).openDrawer();
            },
            icon: const Icon(
              Icons.menu,
              color: AppColors.white,
            ),
          );
        }),
      ),
      drawer: AppDrawer(
        interface: widget.interface,
      ),
      floatingActionButton: Row(
        mainAxisAlignment: MainAxisAlignment.end,
        children: [
          if (widget.interface.currentDay != widget.interface.totalAmountOfDays)
            FloatingActionButton(
              onPressed: () {
                Navigator.pop(context);
              },
              foregroundColor: AppColors.green,
              backgroundColor: AppColors.green,
              child: Icon(
                Icons.restart_alt,
                color: AppColors.white,
                size: 4.h,
              ),
            ),
          SizedBox(
            width: 4.w,
          ),
          FloatingActionButton(
            onPressed: () {
              if (widget.interface.currentDay ==
                  widget.interface.totalAmountOfDays) {
                Navigator.pop(context);
              } else {
                setState(() {
                  widget.interface.nextDayButtonClicked();
                });
              }
            },
            foregroundColor: AppColors.green,
            backgroundColor: AppColors.green,
            child: Icon(
              widget.interface.currentDay == widget.interface.totalAmountOfDays
                  ? Icons.restart_alt
                  : Icons.navigate_next,
              color: AppColors.white,
              size: 4.h,
            ),
          ),
        ],
      ),
      body: Container(
        padding: EdgeInsets.symmetric(horizontal: 4.w, vertical: 1.h),
        child: SingleChildScrollView(
          physics: const BouncingScrollPhysics(),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              TextWithIcon(
                icon: Icons.inventory,
                leftText: 'Упаковки на складе',
                rightText: widget.interface.currentPackages.length.toString(),
              ),
              TextWithIcon(
                icon: Icons.delivery_dining,
                leftText: 'Заказов на доставку',
                rightText: widget.interface.deliveryOrders.length.toString(),
              ),
              TextWithIcon(
                icon: Icons.wrong_location,
                leftText: 'Отмененные заказы на доставку',
                rightText:
                    widget.interface.declinedDeliveryOrders.length.toString(),
              ),
              TextWithIcon(
                icon: Icons.local_shipping,
                leftText: 'Заказы на поставку',
                rightText: widget.interface.supplyOrders.length.toString(),
              ),
              TextWithIcon(
                icon: Icons.all_inbox,
                leftText: 'Отправки на завтра',
                rightText:
                    widget.interface.sendingPackagesTomorrow.length.toString(),
              ),
              TextWithIcon(
                icon: Icons.savings,
                leftText: 'Деньги',
                rightText: widget.interface.moneyAmount.toStringAsFixed(2),
              ),
              TextWithIcon(
                icon: Icons.add_shopping_cart,
                leftText: 'Доход',
                rightText: widget.interface.income.toStringAsFixed(2),
              ),
              TextWithIcon(
                icon: Icons.payments,
                leftText: 'Расход',
                rightText: widget.interface.expenses.toStringAsFixed(2),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
