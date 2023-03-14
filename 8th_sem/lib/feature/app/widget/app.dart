import 'package:flutter/material.dart';
import 'package:package_storage/feature/app/widget/material_context.dart';
import 'package:responsive_sizer/responsive_sizer.dart';

class App extends StatelessWidget {
  const App({Key? key}) : super(key: key);

  static void run() {
    runApp(
      const App(),
    );
  }

  @override
  Widget build(BuildContext context) {
    return ResponsiveSizer(
      builder: (context, orientation, type) {
        return const MaterialContext();
      },
    );
  }
}
