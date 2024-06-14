"""Functions to prevent a nuclear meltdown."""

KELVIN, NEUTRONS_BY_SEC, KELVIN_TIMES_NEUTRONS_BY_SEC = 800, 500, 500_000

def is_criticality_balanced(temperature: float, neutrons_emitted: float):
    """Verify criticality is balanced.

    :param temperature: int or float - temperature value in Kelvin.
    :param neutrons_emitted: int or float - neutrons emitted per second.
    :return: bool - is criticality balanced?

    A reactor is said to be critical if it satisfies the following conditions:
    - The temperature is less than 800 K.
    - The number of neutrons emitted per second is greater than 500.
    - The product of temp. and neutrons emitted per second is less than 500,000.
    """

    return (
        temperature < KELVIN and
        neutrons_emitted > NEUTRONS_BY_SEC and
        temperature * neutrons_emitted < KELVIN_TIMES_NEUTRONS_BY_SEC )


BANDS = (80, 'green'), (60, 'orange'), (30, 'red'), (0, 'black')

def reactor_efficiency(voltage: float, current: float, max_power: float):
    """Assess reactor efficiency zone.

    :param voltage: int or float - voltage value.
    :param current: int or float - current value.
    :param max_power: int or float - theoretical 100% efficiency power.
    :return: str - one of ('green', 'orange', 'red', or 'black').

    Efficiency can be grouped into 4 bands:

    1. green -> efficiency of 80% or more,
    2. orange -> efficiency of less than 80% but at least 60%,
    3. red -> efficiency below 60%, but still 30% or more,
    4. black ->  less than 30% efficient.

    The percentage value is calculated as
    generated power / theoretical max power * 100
    where generated power = voltage * current.
    """

    generated_power = voltage * current
    percent = generated_power / max_power * 100

    for efficiency, color in BANDS:
        if percent >= efficiency: return color

    raise ValueError('Efficiency percentage is less than 0%')


STATUS = 'LOW', 'NORMAL', 'DANGER'; LOW = 90

def fail_safe(temperature: float, neutrons_per_second: float, threshold: float):
    """Assess and return status code for the reactor.

    :param temperature: int or float - value of the temperature in Kelvin.
    :param neutrons_per_second: int or float - neutron flux produced.
    :param threshold: int or float - threshold for category.
    :return: str - one of ('LOW', 'NORMAL', 'DANGER').

    1. 'LOW' -> `temperature * neutrons/s` < 90% of `threshold`.
    2. 'NORMAL' -> `temp. * neutrons/s` +/- 10% `(90 - 110)%` of `threshold`.
    3. 'DANGER' -> `temp. * neutrons/s` isn't in the above-stated ranges.
    """

    prod = temperature * neutrons_per_second; percent = threshold / 100
    low = LOW * percent; normal = (LOW + 20) * percent

    if prod < low : return STATUS[0]
    if low <= prod <= normal: return STATUS[1]

    return STATUS[2]
