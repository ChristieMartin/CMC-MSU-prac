import 'package:flutter/material.dart';
import 'package:package_storage/feature/product/widget/product_screen.dart';
import 'package:package_storage/modeling/product/package.dart';
import 'package:package_storage/widgets/widgets.dart';
import 'package:page_transition/page_transition.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class PackageListScreen extends StatelessWidget {
  const PackageListScreen({
    required this.packages,
    this.discountedPackages = const [],
    this.appbarTitle,
    Key? key,
  }) : super(key: key);
  final List<Package> packages;
  final List<Package> discountedPackages;
  final String? appbarTitle;

  @override
  Widget build(BuildContext context) {
    if (packages.isNotEmpty) {
      packages.sort((p1, p2) => p2.supplyDate.compareTo(p1.supplyDate));
    }
    return Scaffold(
      appBar: AppBar(
        title: Text(
          appbarTitle ?? 'Список упаковок',
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
        children: packages
            .map(
              (e) => _PackageTile(
                e,
                discountedPackages.contains(e),
              ),
            )
            .toList(),
      ),
    );
  }
}

class _PackageTile extends StatefulWidget {
  const _PackageTile(this.package, this.isDiscounted,
      {Key? key})
      : super(key: key);
  final Package package;
  final bool isDiscounted;

  @override
  State<_PackageTile> createState() => _PackageTileState();
}

class _PackageTileState extends State<_PackageTile> {
  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.symmetric(horizontal: 4.w, vertical: 1.h),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              TextButton(
                onPressed: () {
                  Navigator.push(
                    context,
                    PageTransition(
                      type: PageTransitionType.fade,
                      child: ProductScreen(
                        product: widget.package.product,
                      ),
                    ),
                  );
                },
                child: Text(
                  widget.package.product.name,
                  style: TextStyle(
                    fontSize: 18.5.sp,
                    color:
                        widget.isDiscounted ? AppColors.red : AppColors.black,
                    fontWeight: FontWeight.w500,
                  ),
                ),
              ),
              Text(
                '${widget.package.quantity} шт x ${widget.package.product.weight} кг',
                style: TextStyle(
                  fontSize: 18.5.sp,
                  color: AppColors.black,
                ),
              ),
            ],
          ),
          Container(
            margin: EdgeInsets.symmetric(horizontal: 4.w),
            child: Text(
              'Дата поставки: ${widget.package.supplyDate}',
              style: TextStyle(
                fontSize: 18.5.sp,
                color: AppColors.black,
              ),
            ),
          ),
          if (widget.isDiscounted)
            _DiscountSlider(
              package: widget.package,
              onChanged: (val) {
                setState(() {
                  widget.package.discount = val;
                });
              },
            )
        ],
      ),
    );
  }
}

class _DiscountSlider extends StatelessWidget {
  const _DiscountSlider({
    required this.package,
    required this.onChanged,
  });
  final Package package;
  final void Function(double) onChanged;

  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        SizedBox(
          width: 72.w,
          child: AppSlider(
            min: 0.1,
            max: 0.95,
            value: package.discount,
            onChanged: onChanged,
          ),
        ),
        SizedBox(
          width: 1.w,
        ),
        Container(
          width: 18.w,
          padding: EdgeInsets.symmetric(horizontal: 1.w, vertical: 0.5.h),
          decoration: BoxDecoration(
            color: AppColors.green.withOpacity(0.5),
            borderRadius: BorderRadius.circular(1.h),
          ),
          child: Center(
            child: Text(
              package.discount.toStringAsFixed(2),
              style: TextStyle(
                fontSize: 18.5.sp,
                color: AppColors.black,
              ),
            ),
          ),
        )
      ],
    );
  }
}
