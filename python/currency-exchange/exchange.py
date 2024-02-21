"""Functions for calculating steps in exchaning currency.

Python numbers documentation:
https://docs.Python.org/3/library/stdtypes.html#numeric-types-int-float-complex

Overview of exchanging currency when travelling:
https://ComparerEmit.com/money-transfer-tips/guide-to-exchanging-currency-for-overseas-travel
"""

def exchange_money(budget: float, exchange_rate: float):
    """Calculate how much foreign money we can get based on current exchange rate.

    :param budget: float - amount of money you are planning to exchange.
    :param exchange_rate: float - unit value of the foreign currency.
    :return: float - exchanged value of the foreign currency you can receive.
    """

    return budget / exchange_rate


def get_change(budget: float, exchanging_value: float):
    """Show how much money you still have after an exchange operation.

    :param budget: float - amount of money you own.
    :param exchanging_value: float - amount of your money you want to exchange now.
    :return: float - amount left of your starting currency after exchanging.
    """

    return budget - exchanging_value


def get_value_of_bills(denomination: int, number_of_bills: int):
    """Show total money based on number of bills of one type.

    :param denomination: int - the value of a bill.
    :param number_of_bills: int - total number of bills.
    :return: int - calculated value of the bills.
    """

    return denomination * number_of_bills


def get_number_of_bills(amount: float, denomination: int):
    """Calculate how many bills of a kind we'd get for the exchange.

    :param amount: float - the total starting value.
    :param denomination: int - the value of a single bill.
    :return: int - number of bills that can be obtained from the amount.
    """

    return int(amount / denomination)


def get_leftover_of_bills(amount: float, denomination: int):
    """Show how much money we'd lose after the currency bill exchange.

    :param amount: float - the total starting value.
    :param denomination: int - the value of a single bill.
    :return: float - the amount that is "leftover", given the current denomination.
    """

    return amount % denomination


def exchangeable_value(budget: float, exchange_rate: float,
                       spread: int, denomination: int):
    """Calculate the maximum value that can be obtained through currency exchange.

    :param budget: float - the amount of your money you are planning to exchange.
    :param exchange_rate: float - the unit value of the foreign currency.
    :param spread: int - percentage that is taken as an exchange fee.
    :param denomination: int - the value of a single bill.
    :return: int - maximum value you can get.
    """

    exchange_spread_rate = spread / 100 * exchange_rate + exchange_rate
    converted_money = exchange_money(budget, exchange_spread_rate)

    return get_number_of_bills(converted_money, denomination) * denomination
    # return int( converted_money - get_leftover_of_bills(converted_money, denomination) )
