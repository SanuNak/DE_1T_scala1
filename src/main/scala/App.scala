import scala.collection.mutable.Buffer
import scala.io.StdIn.readLine
import scala.math.BigDecimal

object App {
  def main(args: Array[String]): Unit = {
    val helloword = "Hello, Scala!"

    println("******* Решаем задачи из блока 3а **************************************************************** ")
    println(f"Ответ на задачу '3.a.i' следующий: ${helloword.reverse}")
    println(f"Ответ на задачу '3.a.ii' следующий: ${helloword.toLowerCase()}")
    println(f"Ответ на задачу '3.а.iii' следующий: ${helloword.dropRight(1)}")
    println(f"Ответ на задачу '3.а.iv' следующий: ${helloword + " And goodbye python!"}")
    println()


    println("******* Решаем задачу 'b' ************************************************************************ ")
    /* Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
    На вход вашей программе подается значение годового дохода до вычета налогов, размер премии –
    в процентах от годового дохода и компенсация питания. */

    val yearSalary = readLine("Введите годовой оклад одного сотрудника без вычета НДС (например 999), руб.коп: ").toDouble
    val premium = readLine("Введите долю премию , целое число, в процентах (например 22): ").toInt
    val compensation = readLine("Введите величину компенсации за обеды (например 111), руб.коп: ").toDouble
    println()

    // Список ежегодных месячных окладов сотрудников отдела
    val departmentSalary = List[Int](200, 100, 150, 80, 120, 75)
    var monthDepartmentSalary = calculationMonthDepartSalary(premium, compensation, departmentSalary)

    val salary: Double = month_salary(yearSalary, premium, compensation).round
    println(f"Ответ на задачу 'b'. Ежемесячный оклад сотрудника после вычета налогов: $salary%1.2f руб.")
    println()

    println("******* Решаем задачу 'с' ************************************************************************ ")
    /* Напишите программу, которая рассчитывает для каждого сотрудника отклонение (в процентах)
      от среднего значения оклада на уровень всего отдела.
      В итоговом значении должно учитываться в большую или меньшую сторону отклоняется размер оклада.
      На вход вышей программе подаются все значения, аналогичные предыдущей программе,
      а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.*/

    val avg_dep_salary: Int = avg_departament_salary(monthDepartmentSalary)
    val deviation_from_average = deviation(monthDepartmentSalary, avg_dep_salary)
    println(f"Ответ на задачу 'c'. " +
      f"Процентное отклонение ежемесячных оклвдов сотрудников от среднего: ${deviation_from_average.mkString(" ")}")
    println()

    println("******* Решаем задачу 'd' ************************************************************************ ")
    /* Попробуйте рассчитать новую зарплату сотрудника, добавив(или отняв, если сотрудник плохо себя вел)
    необходимую сумму с учетом результатов прошлого задания. Добавьте его зарплату в список и вычислите
    значение самой высокой зарплаты и самой низкой. */

    monthDepartmentSalary = correction(monthDepartmentSalary)
    println()

    println("******* Решаем задачу 'e' ************************************************************************")
    /* Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
      Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему. */

    // Определяем месячные оклады новых работников
    val new_worker_salary = List[Int](350, 90)
    val new_workers_month_salary = calculationMonthDepartSalary(premium, compensation, new_worker_salary)

    // Добавляем месячные оклады новых работников в общий список отеда и сортируем итоговый список
    monthDepartmentSalary ++= new_workers_month_salary
    monthDepartmentSalary = monthDepartmentSalary.sorted
    println()
    println(f"Ответ на задачу '3.e' - добавлены новые ЗП, список отсортирован: ${monthDepartmentSalary.mkString(" ")}")
    println()

    println("******* Решаем задачу 'f' ************************************************************************")
    // Определяем месячный доход нового сотрудника исходя из готового оклада 130 Круб.
    // Определяем нужную позицию и вставляем ЗП туда в общий массив отдела. Обновляем массив по оттделу

    monthDepartmentSalary = again_new_workers(premium, compensation, monthDepartmentSalary)

    println("******* Решаем задачу 'g' ************************************************************************")
    // Определяем месячный доход нового сотрудника исходя из готового оклада 130 Круб.
    // Определяем нужную позицию и вставляем ЗП туда в общий массив отдела. Обновляем массив по оттделу

    val  middleWorkerNumbers: Buffer[Int] = middle_numbers(monthDepartmentSalary)
    println()
    println(f"Ответ на задачу '3.g'. Номера сотрудников с middle ЗП следующие: ${middleWorkerNumbers.mkString(" ")}")
    println(f"Ответ на задачу '3.g'. Для сверки вот общий список: ${monthDepartmentSalary.mkString(" ")}")
    println()


    println("******* Решаем задачу 'h' ************************************************************************")
    /* h. Однако наступил кризис и ваши сотрудники требуют повысить зарплату.
    Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции – 7% */

    monthDepartmentSalary = monthDepartmentSalary.map( _ * 1.07 )
    monthDepartmentSalary = arr_round(monthDepartmentSalary)
    println()
    println(f"Ответ на задачу '3.h'. Вот список ЗП с + 7%%: ${monthDepartmentSalary.mkString(" ")}")
  }

  def arr_round(arr: Buffer[Double]): Buffer[Double] ={
    /* Округляшки значений массива */
    var roundArr = Buffer[Double]()
    for (i <-arr) {
      roundArr += BigDecimal(i).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    roundArr
  }

  def month_salary(yearsalary: Double, premium: Int, compensation: Double) : Double = {
    /* Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
    На вход вашей программе подается значение годового дохода до вычета налогов, размер премии –
    в процентах от годового дохода и компенсация питания. */

    val month_salary_without_nds: Double = (yearsalary - yearsalary * 0.13) / 12
    val month_premium: Double = month_salary_without_nds * premium / 100
    val month_compensation: Double = compensation / 12
    var month_salary: Double = month_salary_without_nds + month_premium + month_compensation
    month_salary = BigDecimal(month_salary).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    return month_salary
  }

  def calculationMonthDepartSalary(premium: Int, compensation: Double, department_salary: List[Int]): Buffer[Double] = {
    // Формируем список ежемесячных окладов сотрудников отдела исходя из их годовых окладов
    val month_salary_list = Buffer[Double]()

    for (i <- department_salary) {
      val salary: Double = month_salary(i, premium, compensation).round
      month_salary_list += salary
    }
    month_salary_list
  }

  def avg_departament_salary(month_department_salary: Buffer[Double]): Int ={
    // Формируем список ежемесячных окладов сотрудников отдела исходя из их годовых окладов
    val month_salary_list = month_department_salary
    // Расчет величины среднего ежемесячного оклада отдела
    val avg_departament_salary = (month_salary_list.sum / month_salary_list.length).toInt

    return avg_departament_salary
  }

  def deviation(month_department_salary: Buffer[Double], avg_dep_salary: Int): Buffer[Int] = {
    /* Напишите программу, которая рассчитывает для каждого сотрудника отклонение (в процентах)
      от среднего значения оклада на уровень всего отдела.
      В итоговом значении должно учитываться в большую или меньшую сторону отклоняется размер оклада.
      На вход вышей программе подаются все значения, аналогичные предыдущей программе,
      а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.*/

    // Получаем отклонения ежемесячных окладов сотрудников от среднего значения оклада отдела
    val deviation_from_average = Buffer[Int]()
    for (i <- month_department_salary) {
      val deviation: Int = ((i - avg_dep_salary) * 100/ avg_dep_salary).toInt
      deviation_from_average += deviation
    }

    return deviation_from_average
  }

  def correction(month_department_salary: Buffer[Double] ): Buffer[Double] = {
    /* Попробуйте рассчитать новую зарплату сотрудника, добавив(или отняв, если сотрудник плохо себя вел)
    необходимую сумму с учетом результатов прошлого задания. Добавьте его зарплату в список и вычислите
    значение самой высокой зарплаты и самой низкой. */

    println()
    val salary_correction: Double = readLine("Введите корректировучную сумму  (например 44), с учетом знака '-': ").toDouble
    val number_employees: Int = month_department_salary.length
    val choose_employee: Int = readLine(f"Введите номер одного из $number_employees сотрудников (например 4): ").toInt

    month_department_salary(choose_employee-1) = month_department_salary(choose_employee-1) + salary_correction

    val max_salary = month_department_salary.max
    val min_salary = month_department_salary.min

    println()
    println(f"Ответ на задачу '3.d' следующий: макс ЗП $max_salary, мин ЗП $min_salary")
    println(f"Ответ на задачу '3.d', также скорректировали ЗП сотрудника №$choose_employee на величину $salary_correction")
    return month_department_salary
  }

  def again_new_workers(premium: Int, compensation: Double, month_department_salary: Buffer[Double]): Buffer[Double] = {
    /* Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч.
    Вычислите самостоятельно номер сотрудника в списке так, чтобы сортировка не нарушилась
    и добавьте его на это место */

    val new_worker_salary: Double = 130
    val new_worker_month_salary: Double = month_salary(new_worker_salary, premium, compensation)
    var salary: Buffer[Double] = month_department_salary

    // Определим место зарплаты нового сотрудника исходя из его месячного окллада
    // Если ЗП меньше первого, то её сразу вставляем в массив на первое место
    if (new_worker_month_salary <= month_department_salary(0)) {
      println()
      println("ЗП нового сотрудника вставили на 1 - ую позицию")
      println()
      new_worker_month_salary +: salary
      salary
    }
    // Если ЗП больше последнего, то её сразу вставляем в массив на последнее место
    else if (new_worker_month_salary >= salary.last) {
      println()
      println("ЗП нового сотрудника вставили на последнюю позицию")
      println()

      salary += new_worker_month_salary
      salary
    }
    else {
      var month_department_salary_new = Buffer[Double]()
      for (i <- salary) {
        if (i > new_worker_month_salary) {
          if (month_department_salary_new.isEmpty) {
            // Хитрым образом разъединяем массив для вставки ЗП нового воркера
            val worker_index = salary.indexOf(i)
            var (front, back) = salary.splitAt(worker_index)

            // Вставляем ЗП нового воркера и по частям собираем новый массив
            front += new_worker_month_salary
            month_department_salary_new = front ++ back

            println()
            println(f"ЗП нового сотрудника вставили на $worker_index - ую позицию")
            println(f"Новые данные по ЗП такие ${month_department_salary_new.mkString(" ")}")
            println()
          }
        }
      }
      month_department_salary_new
    }
  }

  def middle_numbers(salary: Buffer[Double]): Buffer[Int] = {
    /* g. Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle.
    На входе программе подается «вилка» зарплаты специалистов уровня middle. */

    println()
    val upperBound: Double = readLine("Введите верхнюю границу ЗП для middle, руб.коп (например 35): ").toDouble
    val lowerBound: Double = readLine("Введите нижнюю границу ЗП для middle, руб.коп (например 17): ").toDouble
    var middle_numbers = Buffer[Int]()

    for (i <- 0 to salary.size -1 ) {
      if (salary(i) >= lowerBound && salary(i) <= upperBound){
        middle_numbers += i + 1
      }
    }
    middle_numbers
  }
}



