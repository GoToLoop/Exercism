#define C BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE

typedef enum { C } resistor_band_t; static const resistor_band_t c[] = { C };

#define color_code(color) color
#define colors() c
