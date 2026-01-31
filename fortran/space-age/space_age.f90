MODULE space_age; CONTAINS
  ELEMENTAL DOUBLE PRECISION FUNCTION age_in_years(planet, secs) RESULT(age)
    CHARACTER(*), INTENT(IN) :: planet; DOUBLE PRECISION, INTENT(IN) :: secs

    DOUBLE PRECISION, PARAMETER :: SEC = 24*60*60*365.25d0, ORBITS(8) = (/ &
      .2408467d0, .61519726d0, 1d0, 1.8808158d0, 11.862615d0, 29.447498d0, &
      & 84.016846d0, 164.79132d0/)*SEC

    CHARACTER, PARAMETER :: PLANETS(8)*7 = (/"Mercury", "Venus  ", "Earth  ", &
      & "Mars   ", "Jupiter", "Saturn ", "Uranus ", "Neptune"/); age = -1

    i = findloc(PLANETS, planet, 1); IF (i /= 0) age = secs/ORBITS(i); END; END
