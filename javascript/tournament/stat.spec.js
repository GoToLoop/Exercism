// @ts-check

import { StatClass, StatFunc, StatObj } from './stat';

const cls = new StatClass, fun = new StatFunc, obj = StatObj();

describe('Team Stat Constructors', () => {
  test('Instances are instances of their corresponding constructor', () => {
    expect(cls).toBeInstanceOf(StatClass);
    expect(cls).not.toBeInstanceOf(StatFunc);
    expect(cls).not.toBeInstanceOf(StatObj);

    expect(fun).not.toBeInstanceOf(StatClass);
    expect(fun).toBeInstanceOf(StatFunc);
    expect(fun).not.toBeInstanceOf(StatObj);

    expect(obj).not.toBeInstanceOf(StatClass);
    expect(obj).not.toBeInstanceOf(StatFunc);
    expect(obj).not.toBeInstanceOf(StatObj);
  });

  test("Instance's __proto__ points to its corresponding constructor", () => {
    expect(Object.getPrototypeOf(cls)).toBe(StatClass.prototype);
    expect(Object.getPrototypeOf(fun)).toBe(StatFunc.prototype);
    expect(Object.getPrototypeOf(obj)).toBe(Object.prototype);
  });

  test('Method reset() is present in the instance object itself', () => {
    expect(cls.hasOwnProperty('reset')).toBeFalsy();
    expect(fun.hasOwnProperty('reset')).toBeFalsy();
    expect(obj.hasOwnProperty('reset')).toBeTruthy();
  });

  test('Constructors have method total() in their prototype{}', () => {
    expect(StatClass.prototype).toHaveProperty('total');
    expect(StatFunc.prototype).toHaveProperty('total');
    expect(StatObj.prototype).not.toHaveProperty('total');
  });
});
