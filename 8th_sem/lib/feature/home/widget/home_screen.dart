import 'package:flutter/material.dart';
import 'package:package_storage/feature/day/widget/day_screen.dart';
import 'package:package_storage/feature/product/widget/product_screen.dart';
import 'package:package_storage/modeling/interface/interface.dart';
import 'package:package_storage/modeling/product/i_product.dart';
import 'package:package_storage/modeling/random/generator.dart';
import 'package:package_storage/modeling/storage/storage.dart';
import 'package:package_storage/modeling/storage/storage_repository.dart';
import 'package:package_storage/widgets/widgets.dart';
import 'package:page_transition/page_transition.dart';

import 'package:responsive_sizer/responsive_sizer.dart';

class HomeScreen extends StatefulWidget {
  const HomeScreen({super.key});

  @override
  State<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  int _totalAmountOfDays = 10;
  int _stepAmount = 1;
  RangeValues _randomDayValues = const RangeValues(1.4, 3);
  RangeValues _randomQuantity = const RangeValues(3, 10);
  RangeValues _randomSupplyQuantity = const RangeValues(2, 3);
  late List<String> currentAllProducts;
  late List<String> currentAllSalePoints;

  @override
  void initState() {
    currentAllProducts = Generator.allProducts.map((e) => e.name).toList();
    currentAllSalePoints = [...Generator.salePoints];
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Container(
        padding: EdgeInsets.symmetric(
          horizontal: 4.w,
        ),
        child: Column(
          children: [
            SizedBox(
              height: 15.h,
            ),
            _PlayButton(
              onPressed: () {
                List<IProduct> newProducts = Generator.allProducts
                    .where(
                        (element) => currentAllProducts.contains(element.name))
                    .toList();
                Generator.allProducts = newProducts;
                Generator.salePoints = currentAllSalePoints;

                Generator.randomDayValuesMin = _randomDayValues.start.round();
                Generator.randomDayValuesMax = _randomDayValues.end.round();

                Generator.randomQuantityMin = _randomQuantity.start.round();
                Generator.randomQuantityMax = _randomQuantity.end.round();

                Generator.randomSupplyQuantityMin =
                    _randomSupplyQuantity.start.round();
                Generator.randomSupplyQuantityMin =
                    _randomSupplyQuantity.end.round();
                Storage storage = Storage(
                  packages: [],
                  productsInfo: Generator.getInitProductInfos(),
                );

                StorageRepository repo = StorageRepository(
                  storage: storage,
                );
                repo.addProductList(Generator.allProducts, 0);

                Navigator.push(
                  context,
                  PageTransition(
                    type: PageTransitionType.fade,
                    child: DayScreen(
                      interface: Interface(
                        totalAmountOfDays: _totalAmountOfDays,
                        stepAmount: _stepAmount,
                        storageRepository: repo,
                      ),
                    ),
                  ),
                );
              },
            ),
            Container(
              margin: EdgeInsets.symmetric(
                vertical: 2.h,
              ),
              child: Text(
                'Начать моделирование',
                style: TextStyle(
                  fontSize: 20.sp,
                  fontWeight: FontWeight.w500,
                  color: AppColors.black,
                ),
              ),
            ),
            const AppDivider(),
            Expanded(
              child: Container(
                padding: EdgeInsets.symmetric(
                  horizontal: 4.w,
                ),
                child: SingleChildScrollView(
                  physics: const BouncingScrollPhysics(),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      _SliderWithText(
                        text: 'Период моделирования',
                        min: 10,
                        max: 30,
                        value: _totalAmountOfDays,
                        onChanged: (newValue) {
                          setState(() {
                            _totalAmountOfDays = newValue.round();
                          });
                        },
                      ),
                      _SliderWithText(
                        text: 'Шаг моделирования',
                        min: 1,
                        max: 10,
                        value: _stepAmount,
                        onChanged: (newValue) {
                          setState(() {
                            _stepAmount = newValue.round();
                          });
                        },
                      ),
                      _SliderWithText(
                        text: 'Количество дней',
                        min: 1,
                        max: 10,
                        values: _randomDayValues,
                        onChanged: (newValue) {
                          setState(() {
                            _randomDayValues = RangeValues(
                              newValue.start,
                              newValue.end,
                            );
                          });
                        },
                      ),
                      _SliderWithText(
                        text: 'Количество в \nзаказе/упаковке',
                        min: 1,
                        max: 20,
                        values: _randomQuantity,
                        onChanged: (newValue) {
                          setState(() {
                            _randomQuantity = RangeValues(
                              newValue.start,
                              newValue.end,
                            );
                          });
                        },
                      ),
                      _SliderWithText(
                        text: 'Количество товаров\nв заказе',
                        min: 1,
                        max: 5,
                        values: _randomSupplyQuantity,
                        onChanged: (newValue) {
                          setState(() {
                            _randomSupplyQuantity = RangeValues(
                              newValue.start,
                              newValue.end,
                            );
                          });
                        },
                      ),
                      _ItemChooser(
                        topText: 'Продуктовые товары',
                        items:
                            Generator.allProducts.map((e) => e.name).toList(),
                        currentItems: currentAllProducts,
                        min: 12,
                        onCheckmark: (item) {
                          if (currentAllProducts.length == 12 &&
                              currentAllProducts.contains(item)) {
                            return;
                          }
                          setState(() {
                            if (currentAllProducts.contains(item)) {
                              currentAllProducts.remove(item);
                            } else {
                              currentAllProducts.add(item);
                            }
                          });
                        },
                        onItemTapped: (item) {
                          Navigator.push(
                            context,
                            PageTransition(
                              type: PageTransitionType.fade,
                              child: ProductScreen(
                                product: Generator.allProducts
                                    .where((element) => element.name == item)
                                    .first,
                              ),
                            ),
                          );
                        },
                      ),
                      _ItemChooser(
                        topText: 'Торговые точки',
                        items: Generator.salePoints,
                        min: 3,
                        currentItems: currentAllSalePoints,
                        onCheckmark: (item) {
                          if (currentAllSalePoints.length == 3 &&
                              currentAllSalePoints.contains(item)) {
                            return;
                          }
                          setState(() {
                            if (currentAllSalePoints.contains(item)) {
                              currentAllSalePoints.remove(item);
                            } else {
                              currentAllSalePoints.add(item);
                            }
                          });
                        },
                        onItemTapped: (item) {},
                      ),
                    ],
                  ),
                ),
              ),
            )
          ],
        ),
      ),
    );
  }
}

class _ItemChooser extends StatefulWidget {
  const _ItemChooser({
    required this.topText,
    required this.items,
    required this.min,
    required this.currentItems,
    required this.onItemTapped,
    required this.onCheckmark,
  });
  final String topText;
  final int min;
  final List<String> items;
  final List<String> currentItems;
  final void Function(String) onItemTapped;
  final void Function(String) onCheckmark;

  @override
  State<_ItemChooser> createState() => __ItemChooserState();
}

class __ItemChooserState extends State<_ItemChooser> {
  bool _isOpen = false;

  @override
  Widget build(BuildContext context) {
    return AnimatedContainer(
      duration: const Duration(milliseconds: 300),
      margin: EdgeInsets.symmetric(
        vertical: 1.h,
      ),
      height:
          _isOpen ? (widget.items.length / 2).round() * 5.h + 8.h : 20.h + 2.h,
      decoration: BoxDecoration(
        color: AppColors.darkGreen.withOpacity(0.2),
        borderRadius: BorderRadius.circular(2.h),
      ),
      child: Column(
        children: [
          Container(
            height: 6.h,
            padding: EdgeInsets.symmetric(
              horizontal: 4.w,
              vertical: 1.h,
            ),
            decoration: BoxDecoration(
              color: AppColors.darkGreen.withOpacity(0.2),
              borderRadius: BorderRadius.only(
                topLeft: Radius.circular(2.h),
                topRight: Radius.circular(2.h),
              ),
            ),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Text(
                  widget.topText,
                  style: TextStyle(
                    fontSize: 18.5.sp,
                    color: AppColors.black,
                  ),
                ),
                RichText(
                  text: TextSpan(
                    children: [
                      TextSpan(
                        text: '${widget.currentItems.length}',
                        style: TextStyle(
                          color: widget.min == widget.currentItems.length
                              ? AppColors.red
                              : AppColors.black,
                        ),
                      ),
                      TextSpan(text: '/${widget.items.length}')
                    ],
                    style: TextStyle(
                      fontFamily: 'Rubik',
                      fontSize: 18.5.sp,
                      color: AppColors.black,
                    ),
                  ),
                ),
              ],
            ),
          ),
          if (_isOpen)
            Expanded(
              child: _grid(
                _isOpen,
              ),
            ),
          if (!_isOpen)
            SizedBox(
              height: 12.h,
              child: _grid(
                _isOpen,
              ),
            ),
          if (!_isOpen)
            Expanded(
              child: TextButton(
                style: TextButton.styleFrom(
                  padding: EdgeInsets.zero,
                ),
                onPressed: () {
                  setState(() {
                    _isOpen = true;
                  });
                },
                child: Container(
                  height: 4.h,
                  decoration: BoxDecoration(
                    color: AppColors.darkGreen.withOpacity(0.2),
                    borderRadius: BorderRadius.only(
                      bottomLeft: Radius.circular(2.h),
                      bottomRight: Radius.circular(2.h),
                    ),
                  ),
                  child: Center(
                    child: Icon(
                      Icons.arrow_drop_down,
                      color: AppColors.white,
                      size: 4.h,
                    ),
                  ),
                ),
              ),
            ),
        ],
      ),
    );
  }

  Widget _grid(bool isOpen) {
    return GridView(
      padding: EdgeInsets.symmetric(horizontal: 4.w, vertical: 1.h),
      shrinkWrap: true,
      physics: const NeverScrollableScrollPhysics(),
      gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
        crossAxisCount: 2,
        childAspectRatio: 2,
        mainAxisExtent: 5.h,
      ),
      children: widget.items
          .take(isOpen ? widget.items.length : 4)
          .map(
            (e) => _ChoosableItem(
              itemName: e,
              isSelected: widget.currentItems.contains(e),
              onChoosed: widget.onCheckmark,
              onTextPressed: widget.onItemTapped,
            ),
          )
          .toList(),
    );
  }
}

class _ChoosableItem extends StatelessWidget {
  const _ChoosableItem({
    required this.itemName,
    required this.isSelected,
    required this.onChoosed,
    required this.onTextPressed,
  });
  final String itemName;
  final bool isSelected;
  final void Function(String) onChoosed;
  final void Function(String) onTextPressed;
  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: 5.h,
      child: Row(
        children: [
          SizedBox(
            width: 5.h,
            child: TextButton(
              onPressed: () {
                onChoosed(itemName);
              },
              style: TextButton.styleFrom(
                padding: EdgeInsets.zero,
              ),
              child: _Checkmark(
                isSelected: isSelected,
              ),
            ),
          ),
          SizedBox(
            width: 2.w,
          ),
          TextButton(
            onPressed: () {
              onTextPressed(itemName);
            },
            style: TextButton.styleFrom(
              padding: EdgeInsets.zero,
              alignment: Alignment.centerLeft,
            ),
            child: Text(
              itemName,
              textAlign: TextAlign.start,
              style: TextStyle(
                fontSize: 18.sp,
                fontWeight: FontWeight.w400,
                color: AppColors.black,
              ),
            ),
          )
        ],
      ),
    );
  }
}

class _Checkmark extends StatelessWidget {
  const _Checkmark({
    required this.isSelected,
  });
  final bool isSelected;

  @override
  Widget build(BuildContext context) {
    return Container(
      decoration: BoxDecoration(
        color: isSelected ? AppColors.green : AppColors.green.withOpacity(0.5),
        borderRadius: BorderRadius.circular(1.h),
      ),
      padding: EdgeInsets.all(1.w),
      child: Icon(
        Icons.check,
        color: isSelected ? AppColors.white : Colors.transparent,
        size: 2.5.h,
      ),
    );
  }
}

class _SliderWithText extends StatelessWidget {
  const _SliderWithText({
    required this.text,
    required this.min,
    required this.max,
    this.value,
    this.values,
    required this.onChanged,
  });
  final String text;
  final int min;
  final int max;
  final int? value;
  final RangeValues? values;
  final void Function(dynamic) onChanged;

  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.symmetric(vertical: 1.h),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            text,
            style: TextStyle(
              fontSize: 18.5.sp,
              color: AppColors.black,
            ),
          ),
          Row(
            children: [
              Expanded(
                child: SliderTheme(
                  data: SliderTheme.of(context).copyWith(
                    overlayShape: SliderComponentShape.noOverlay,
                    rangeThumbShape: RoundRangeSliderThumbShape(
                      enabledThumbRadius: 3.w,
                      elevation: 0,
                    ),
                    thumbShape: RoundSliderThumbShape(
                      enabledThumbRadius: 3.w,
                      elevation: 0,
                    ),
                    thumbColor: AppColors.green,
                    disabledThumbColor: AppColors.green,
                  ),
                  child: value != null
                      ? AppSlider(
                          min: min.toDouble(),
                          max: max.toDouble(),
                          value: value!.toDouble(),
                          onChanged: onChanged,
                        )
                      : RangeSlider(
                          inactiveColor: AppColors.lightBrown.withOpacity(0.5),
                          activeColor: AppColors.lightBrown,
                          min: min.toDouble(),
                          max: max.toDouble(),
                          values: values!,
                          //thumbColor: AppColors.green,
                          onChanged: onChanged,
                        ),
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
                    value != null
                        ? '$value/$max'
                        : '${values!.start.round()}-${values!.end.round()}',
                    style: TextStyle(
                      fontSize: 18.5.sp,
                      color: AppColors.black,
                    ),
                  ),
                ),
              )
            ],
          )
        ],
      ),
    );
  }
}

class _PlayButton extends StatelessWidget {
  const _PlayButton({
    required this.onPressed,
  });
  final VoidCallback onPressed;

  @override
  Widget build(BuildContext context) {
    return Center(
      child: TextButton(
        onPressed: onPressed,
        style: TextButton.styleFrom(
          padding: EdgeInsets.zero,
        ),
        child: Container(
          height: 15.h,
          width: 15.h,
          alignment: Alignment.center,
          decoration: const BoxDecoration(
            shape: BoxShape.circle,
            color: AppColors.green,
          ),
          child: Icon(
            Icons.play_arrow,
            size: 10.h,
            color: AppColors.white,
          ),
        ),
      ),
    );
  }
}
