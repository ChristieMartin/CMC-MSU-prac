import 'package:flutter/material.dart';
import 'package:package_storage/feature/order/wigdet/delivery_order_list_screen.dart';
import 'package:package_storage/feature/order/wigdet/supply_order_list_screen.dart';
import 'package:package_storage/feature/package/widget/package_list_screen.dart';
import 'package:package_storage/modeling/interface/interface.dart';
import 'package:package_storage/widgets/widgets.dart';
import 'package:page_transition/page_transition.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class AppDrawer extends StatelessWidget {
  const AppDrawer({
    required this.interface,
    Key? key,
  }) : super(key: key);
  final Interface interface;

  @override
  Widget build(BuildContext context) {
    return Drawer(
      backgroundColor: AppColors.darkGreen,
      child: Column(
        children: [
          SizedBox(
            height: 15.h,
          ),
          _MenuItem(
            text: 'Упаковки',
            icon: Icons.inventory,
            onPressed: () {
              Scaffold.of(context).closeDrawer();
              Navigator.push(
                context,
                PageTransition(
                  type: PageTransitionType.fade,
                  child: PackageListScreen(
                    packages: interface.currentPackages,
                    discountedPackages: interface.discountedPackages,
                  ),
                ),
              );
            },
          ),
          _MenuItem(
            text: 'Просроченные упаковки',
            icon: Icons.close_outlined,
            onPressed: () {
              Scaffold.of(context).closeDrawer();
              Navigator.push(
                context,
                PageTransition(
                  type: PageTransitionType.fade,
                  child: PackageListScreen(
                    packages: interface.expiredPackages,
                  ),
                ),
              );
            },
          ),
          _MenuItem(
            text: 'Заказы на доставку',
            icon: Icons.delivery_dining,
            onPressed: () {
              Scaffold.of(context).closeDrawer();
              Navigator.push(
                context,
                PageTransition(
                  type: PageTransitionType.fade,
                  child: DeliveryOrderListScreen(
                    orders: interface.deliveryOrders,
                  ),
                ),
              );
            },
          ),
          _MenuItem(
            text: 'Заказы на поставку',
            icon: Icons.local_shipping,
            onPressed: () {
              Scaffold.of(context).closeDrawer();
              Navigator.push(
                context,
                PageTransition(
                  type: PageTransitionType.fade,
                  child: SupplyOrderListScreen(
                    orders: interface.supplyOrders,
                  ),
                ),
              );
            },
          ),
          _MenuItem(
            text: 'Отправки на завтра',
            icon: Icons.all_inbox,
            onPressed: () {
              Scaffold.of(context).closeDrawer();
              Navigator.push(
                context,
                PageTransition(
                  type: PageTransitionType.fade,
                  child: PackageListScreen(
                    appbarTitle: 'Отправки на завтра',
                    packages: interface.sendingPackagesTomorrow,
                  ),
                ),
              );
            },
          ),
        ],
      ),
    );
  }
}

class _MenuItem extends StatelessWidget {
  const _MenuItem({
    required this.onPressed,
    required this.icon,
    required this.text,
  });
  final VoidCallback onPressed;
  final IconData icon;
  final String text;

  @override
  Widget build(BuildContext context) {
    return TextButton(
      style: TextButton.styleFrom(
        padding: EdgeInsets.symmetric(
          horizontal: 4.w,
          vertical: 1.25.h,
        ),
      ),
      onPressed: onPressed,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          Row(
            children: [
              SizedBox(
                width: 10.w,
                child: Icon(
                  icon,
                  size: 10.w,
                  color: AppColors.white,
                ),
              ),
              SizedBox(
                width: 4.w,
              ),
              Text(
                text,
                style: TextStyle(
                  color: AppColors.white,
                  fontSize: 17.sp,
                ),
              ),
            ],
          ),
        ],
      ),
    );
  }
}
