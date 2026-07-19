#!/usr/bin/env python3

import argparse
from dataclasses import dataclass
from datetime import date, timedelta
import sys

# output language, "en" or "ru"; set from --lang
LANG = "en"

# meals of a full day, in serving order
MEALS = ["breakfast", "lunch", "snack", "supper"]

# product types for the grouped total report, in purchase order
PRODUCT_TYPES = ["grocery", "sublimates", "tushenka", "dried fruits & nuts",
                 "cheese", "sausage", "crackers", "sweets", "others"]

@dataclass
class Product:
    name: str
    name_ru: str
    # in grams
    weight: int
    # one of PRODUCT_TYPES
    ptype: str
    # bulk density in g/ml for products packed by volume (grains, sugar etc);
    # 0 means volume is not reported
    density: float = 0.0

    @property
    def display_name(self) -> str:
        return self.name_ru if LANG == "ru" else self.name

# see katz at
# https://www.mountain.ru/legacy-material.php?article_id=3702

# breakfasts
# katz: 65-80
oat = Product("oat", "овсянка", 50, "grocery", density=0.4)
semolina = Product("semolina", "манка", 50, "grocery", density=0.7)
# katz: 65-80
# round-grain (кубанский) rice
rice_porridge = Product("rice porridge", "рисовая каша", 60, "grocery", density=0.9)
# katz: 80-95
millet = Product("millet", "пшёнка", 50, "grocery", density=0.85)
# кукурузная крупа (grits, not flakes)
corn_porridge = Product("corn porridge", "кукурузная каша", 50, "grocery", density=0.75)
cereal = Product("cereal", "хлопья", 50, "grocery", density=0.35)

# often apricots and raisins go to porridge, the rest is for snacking
# katz: 35-50
# nastya: 25
DRIED_FRUITS_PORTION = 35
dried_apricots = Product("dried apricots", "курага", DRIED_FRUITS_PORTION, "dried fruits & nuts")
raisins = Product("raisins", "изюм", DRIED_FRUITS_PORTION, "dried fruits & nuts")

# katz: 15-30
# nastya: 15
dry_milk = Product("dry milk", "сухое молоко", 25, "grocery", density=0.5)

ghee = Product("ghee", "топлёное масло", 5, "others")

porridge_sugar = Product("porridge sugar", "сахар в кашу", 15, "others", density=0.85)

# katz: 20-40g (about 2 pieces)
white_crackers = Product("white crackers", "белые сухари", 30, "crackers")

# katz: 25-40
# oltermanni is nice
cheese = Product("cheese", "сыр", 35, "cheese")

# lunch + snack

# old style folks often take 20g dry soup + 10g prysypka
dry_soup = Product("dry soup", "сухой суп", 20, "sublimates")
# specdetal sublimated soup is about 70g.
sublimated_soup = Product("sublimated soup", "сублимированный суп", 70, "sublimates")

onion = Product("onion", "лук", 10, "others")

dry_sausage = Product("dry sausage", "колбаса", 35, "sausage")
# finn crisps are nice
black_crackers = Product("black crackers", "чёрные сухари", 30, "crackers")
prunes = Product("prunes", "чернослив", DRIED_FRUITS_PORTION, "dried fruits & nuts")
dates = Product("dates", "финики", DRIED_FRUITS_PORTION, "dried fruits & nuts")
dried_cherry = Product("dried cherry", "сушёная вишня", DRIED_FRUITS_PORTION, "dried fruits & nuts")
# katz: 20-35
nuts = Product("nuts", "орехи", 30, "dried fruits & nuts")
# 15-25
chocolate = Product("chocolate", "шоколад", 30, "sweets")

# supper
# nastya: 90
# basmati, packs looser than round rice
rice = Product("rice", "рис", 90, "grocery", density=0.75)
buckwheat = Product("buckwheat", "гречка", 90, "grocery", density=0.8)
# katz: 95-100
pasta = Product("pasta", "макароны", 90, "grocery", density=0.45)
# + dry milk 25
karpur = Product("karpur", "карпюр", 60, "grocery", density=0.25)
lentils = Product("lentils", "чечевица", 90, "grocery", density=0.85)

# tushenka goes about 40-75g. sublimated meat 15-30g.
# specdetal 'big portion' dishes are 120g; side is included, so we don't need or reduce grain.
tushenka = Product("tushenka", "тушёнка", 40, "tushenka")
sublimated_meat = Product("sublimated meat", "сублимированное мясо", 25, "sublimates")
sublimated_dish = Product("sublimated dish", "сублимированное блюдо", 120, "sublimates")

# sweets
# katz: 35-50
SWEETS_PORTION = 30
halva = Product("halva", "халва", SWEETS_PORTION, "sweets")
pryaniks = Product("pryaniks", "пряники", SWEETS_PORTION, "sweets")
cookies = Product("cookies", "печенье", SWEETS_PORTION, "sweets")
candies = Product("candies", "конфеты", SWEETS_PORTION, "sweets")

# common per person per day
# katz: 5-10
tea = Product("tea", "чай", 10, "others")
# Separate from morning porridge (to tea). katz: 10-20
sugar = Product("sugar", "сахар", 15, "others", density=0.85)
salt = Product("salt", "соль", 6, "others", density=1.2)
ketchup = Product("ketchup", "кетчуп", 5, "others")

# Notes:
# May also add onion/garlic to lunch/supper.

MSG = {
    "en": {
        "per_person": "Per person per meal weights:",
        "bf_porridge": "Breakfast porridge (cycled)",
        "bf_dried_fruits": "Breakfast dried fruits (cycled)",
        "bf_extras": "Breakfast extras",
        "lunch_soup": "Lunch soup",
        "lunch_extras": "Lunch extras",
        "snack_dried_fruits": "Snack dried fruits (cycled)",
        "snack_extras": "Snack extras",
        "supper_dish": "Supper dish",
        "supper_main": "Supper main (cycled)",
        "supper_meat": "Supper meat",
        "supper_extras": "Supper extras",
        "supper_sweets": "Supper sweets (cycled)",
        "common_daily": "Common daily per person",
        "breakfast": "Breakfast",
        "lunch": "Lunch",
        "snack": "Snack",
        "supper": "Supper",
        "common": "Common",
        "day": "Day",
        "total": "Total",
        "everything": "everything",
        "g": "g",
        "kg": "kg",
        "ml": "ml",
        "header": "{} days with {} full and {} half portions.",
    },
    "ru": {
        "per_person": "Вес продуктов на человека на приём пищи:",
        "bf_porridge": "Каша на завтрак (по кругу)",
        "bf_dried_fruits": "Сухофрукты на завтрак (по кругу)",
        "bf_extras": "Дополнительно на завтрак",
        "lunch_soup": "Суп на обед",
        "lunch_extras": "Дополнительно на обед",
        "snack_dried_fruits": "Сухофрукты на перекус (по кругу)",
        "snack_extras": "Дополнительно на перекус",
        "supper_dish": "Блюдо на ужин",
        "supper_main": "Гарнир на ужин (по кругу)",
        "supper_meat": "Мясо на ужин",
        "supper_extras": "Дополнительно на ужин",
        "supper_sweets": "Сладкое на ужин (по кругу)",
        "common_daily": "Общее на человека в день",
        "breakfast": "Завтрак",
        "lunch": "Обед",
        "snack": "Перекус",
        "supper": "Ужин",
        "common": "Общее",
        "day": "День",
        "total": "Итого",
        "everything": "всего",
        "g": "г",
        "kg": "кг",
        "ml": "мл",
        "header": "Дней: {}, полных порций: {}, половинных: {}.",
    },
}

TYPE_NAMES_RU = {
    "grocery": "бакалея",
    "sublimates": "сублиматы",
    "tushenka": "тушёнка",
    "dried fruits & nuts": "сухофрукты и орехи",
    "cheese": "сыр",
    "sausage": "колбаса",
    "crackers": "сухари",
    "sweets": "сладкое",
    "others": "прочее",
}

def msg(key: str) -> str:
    return MSG[LANG][key]

def type_name(ptype: str) -> str:
    return TYPE_NAMES_RU[ptype] if LANG == "ru" else ptype

def format_products(products: list[Product]) -> str:
    return ", ".join(f"{p.display_name} {p.weight}{msg('g')}" for p in products)

class TableCloth:
    """Table cloth for raskladka."""
    def __init__(self, portions: int, half_portions: int = 0, supper_mode: str = "sub-dish",
                 start_date: date | None = None):
        self.portions = portions
        self.half_portions = half_portions
        # if set, days in the menu are annotated with dates
        self.start_date = start_date
        # one of "tushenka" (side + tushenka), "sub-meat" (side + sublimated
        # meat), "sub-dish" (ready sublimated dish, side included)
        self.supper_mode = supper_mode
        # Control menu here.
        # Breakfasts:
        self.breakfasts = [oat, semolina, rice_porridge, millet]
        self.breakfast_ptr = 0
        self.bf_dried_fruits = [dried_apricots, raisins]
        self.bf_dried_fruits_ptr = 0
        # served every breakfast
        self.bf_extras = [dry_milk, ghee, porridge_sugar, white_crackers, cheese]
        # Lunches:
        self.lunch_soup = sublimated_soup
        # served every lunch
        self.lunch_extras = [dry_sausage, black_crackers, onion]
        # Snacks:
        self.snack_dried_fruits = [prunes, dates, dried_cherry]
        self.snack_dried_fruits_ptr = 0
        # served every snack
        self.snack_extras = [nuts, chocolate]
        # Suppers:
        self.suppers = [rice, buckwheat, pasta, karpur]
        self.supper_ptr = 0
        self.sweets = [halva, pryaniks, cookies, candies]
        self.sweets_ptr = 0
        # served every supper
        self.supper_extras = [ghee]
        # served every day
        self.common = [tea, sugar, salt, ketchup]

        self.menu_report = msg("per_person") + "\n"
        self.menu_report += f"{msg('bf_porridge')}: {format_products(self.breakfasts)}\n"
        self.menu_report += f"{msg('bf_dried_fruits')}: {format_products(self.bf_dried_fruits)}\n"
        self.menu_report += f"{msg('bf_extras')}: {format_products(self.bf_extras)}\n"
        self.menu_report += f"{msg('lunch_soup')}: {format_products([self.lunch_soup])}\n"
        self.menu_report += f"{msg('lunch_extras')}: {format_products(self.lunch_extras)}\n"
        self.menu_report += f"{msg('snack_dried_fruits')}: {format_products(self.snack_dried_fruits)}\n"
        self.menu_report += f"{msg('snack_extras')}: {format_products(self.snack_extras)}\n"
        if self.supper_mode == "sub-dish":
            self.menu_report += f"{msg('supper_dish')}: {format_products([sublimated_dish])}\n"
        else:
            self.menu_report += f"{msg('supper_main')}: {format_products(self.suppers)}\n"
            meat = tushenka if self.supper_mode == "tushenka" else sublimated_meat
            self.menu_report += f"{msg('supper_meat')}: {format_products([meat])}\n"
        self.menu_report += f"{msg('supper_extras')}: {format_products(self.supper_extras)}\n"
        self.menu_report += f"{msg('supper_sweets')}: {format_products(self.sweets)}\n"
        self.menu_report += f"\n{msg('common_daily')}: {format_products(self.common)}\n"

        # days counter
        self.days = 0
        self.daily_report = ""
        # total goods, product name -> {serving weight in grams: count}
        self.warehouse: dict[str, dict[int, int]] = {}
        # product name -> Product, for grouping the total report by type
        self.warehouse_products: dict[str, Product] = {}

    def add_product(self, product: Product, halved: bool = False) -> str:
        """Account product in the warehouse, return its menu entry.

        Half portions get half of halved products (grains and dry milk),
        but a full share of everything else.
        """
        if halved:
            weight = product.weight * self.portions + product.weight * self.half_portions // 2
        else:
            weight = product.weight * (self.portions + self.half_portions)
        servings = self.warehouse.setdefault(product.name, {})
        servings[weight] = servings.get(weight, 0) + 1
        self.warehouse_products[product.name] = product
        return f"{product.display_name} {weight}{msg('g')}"

    def add_breakfast(self):
        """Add breakfast: porridge with dried fruits plus daily extras"""
        porridge = self.breakfasts[self.breakfast_ptr]
        self.breakfast_ptr = (self.breakfast_ptr + 1) % len(self.breakfasts)
        fruits = self.bf_dried_fruits[self.bf_dried_fruits_ptr]
        self.bf_dried_fruits_ptr = (self.bf_dried_fruits_ptr + 1) % len(self.bf_dried_fruits)

        entries = [self.add_product(porridge, halved=True)]
        entries += [self.add_product(p, halved=(p is dry_milk)) for p in [fruits] + self.bf_extras]
        self.daily_report += f"{msg('breakfast')}: " + ", ".join(entries) + "\n"

    def add_lunch(self):
        """Add lunch: soup plus daily extras"""
        entries = [self.add_product(self.lunch_soup, halved=True)]
        entries += [self.add_product(p) for p in self.lunch_extras]
        self.daily_report += f"{msg('lunch')}: " + ", ".join(entries) + "\n"

    def add_snack(self):
        """Add snack: dried fruits plus daily extras"""
        fruits = self.snack_dried_fruits[self.snack_dried_fruits_ptr]
        self.snack_dried_fruits_ptr = (self.snack_dried_fruits_ptr + 1) % len(self.snack_dried_fruits)

        entries = [self.add_product(p) for p in [fruits] + self.snack_extras]
        self.daily_report += f"{msg('snack')}: " + ", ".join(entries) + "\n"

    def add_supper(self):
        """Add supper: main course with sweets"""
        sweets = self.sweets[self.sweets_ptr]
        self.sweets_ptr = (self.sweets_ptr + 1) % len(self.sweets)

        entries = []
        if self.supper_mode == "sub-dish":
            entries.append(self.add_product(sublimated_dish, halved=True))
        else:
            main = self.suppers[self.supper_ptr]
            self.supper_ptr = (self.supper_ptr + 1) % len(self.suppers)
            entries.append(self.add_product(main, halved=True))
            if main is karpur:
                entries.append(self.add_product(dry_milk, halved=True))
            meat = tushenka if self.supper_mode == "tushenka" else sublimated_meat
            entries.append(self.add_product(meat, halved=True))
        entries += [self.add_product(p) for p in self.supper_extras]
        entries.append(self.add_product(sweets))
        self.daily_report += f"{msg('supper')}: " + ", ".join(entries) + "\n"

    def add_day(self, first_meal: str = "breakfast", last_meal: str = "supper"):
        """Add a day serving meals from first_meal to last_meal inclusive
        (in MEALS order); common daily products are always served."""
        self.days += 1
        day_header = f"\n{msg('day')} {self.days}"
        if self.start_date is not None:
            day_date = self.start_date + timedelta(days=self.days - 1)
            day_header += f" ({day_date.day:02d}.{day_date.month:02d})"
        self.daily_report += day_header + ":\n"
        meal_adders = {"breakfast": self.add_breakfast, "lunch": self.add_lunch,
                       "snack": self.add_snack, "supper": self.add_supper}
        for meal in MEALS[MEALS.index(first_meal):MEALS.index(last_meal) + 1]:
            meal_adders[meal]()
        entries = [self.add_product(p) for p in self.common]
        self.daily_report += f"{msg('common')}: " + ", ".join(entries) + "\n"

    def add_days(self, days: int, first_day: str | None = None, last_day: str | None = None):
        """Add full days, optionally surrounded by partial days: first_day
        starts the trip with that meal, last_day ends it with that meal."""
        if first_day is not None:
            self.add_day(first_meal=first_day)
        for _ in range(days):
            self.add_day()
        if last_day is not None:
            self.add_day(last_meal=last_day)

    def report(self) -> str:
        """Meal weights, daily menu, total goods to buy."""
        result = self.menu_report + "\n"
        result += self.daily_report
        result += f"\n{msg('total')}:\n"
        # group purchases by product type, in PRODUCT_TYPES order
        for ptype in PRODUCT_TYPES:
            products = [p for p in self.warehouse_products.values() if p.ptype == ptype]
            if not products:
                continue
            result += f"{type_name(ptype)}:\n"
            for p in products:
                # servings maps serving weight -> number of such servings,
                # e.g. {62: 5} is 5 meals needing a 62g pack each
                servings = self.warehouse[p.name]
                grams = sum(w * c for w, c in servings.items())
                # per-pack breakdown to ease packing: "5 x 62g", or
                # "5 x 15g + 2 x 10g" if serving weights differ between meals;
                # bulk products additionally get per-pack volume, "5 x 100g ~ 250ml"
                supply_parts = []
                for w, c in servings.items():
                    part = f"{c} x {w}{msg('g')}"
                    if p.density:
                        part += f" ~ {round(w / p.density)}{msg('ml')}"
                    supply_parts.append(part)
                supply = " + ".join(supply_parts)
                result += f"  {p.display_name}: {grams}{msg('g')} ({supply})\n"
        total = sum(w * c for servings in self.warehouse.values() for w, c in servings.items())
        result += f"\n{msg('everything')}: {total / 1000:.2f}{msg('kg')}\n"
        return result

if __name__ == '__main__':
    description = """
    Calculate raskladka.
    Usage example:
    ./raskladka.py -d 10 -p 2
    """
    parser = argparse.ArgumentParser(description=description, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("-d", "--days", help="full days", type=int, required=True)
    parser.add_argument("-p", "--portions", help="number of full portions", type=int, default=10)
    parser.add_argument("--half-portions", help="number of half portions (half of grains, rest full; often used for kids)", type=int, default=0)
    parser.add_argument("--supper", help="supper mode: side + tushenka, side + sublimated meat, or ready sublimated dish",
                        choices=["tushenka", "sub-meat", "sub-dish"], default="sub-dish")
    parser.add_argument("-l", "--lang", "--language", help="output language", choices=["en", "ru"], default="en")
    parser.add_argument("--start-date", help="trip start date as YYYY-MM-DD; annotates menu days with dates",
                        type=date.fromisoformat)
    parser.add_argument("--first-day", help="prepend a partial day starting with this meal",
                        choices=["lunch", "supper"])
    parser.add_argument("--last-day", help="append a partial day ending with this meal",
                        choices=["breakfast", "lunch"])
    args = parser.parse_args()
    LANG = args.lang
    print(msg("header").format(args.days, args.portions, args.half_portions) + "\n")

    tablecloth = TableCloth(args.portions, args.half_portions, args.supper, args.start_date)
    tablecloth.add_days(args.days, args.first_day, args.last_day)
    print(tablecloth.report())
