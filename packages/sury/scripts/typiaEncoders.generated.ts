// @ts-nocheck
// Generated from typiaEncoders.template.ts — do not edit by hand.
//
// typia expands its `typia.json.*` calls in a TypeScript transform, which this
// repo does not run. Checking the expansion in is what lets `bench:jsonstring`
// compare against typia through the same tsx entry point as everything else.
// Regenerate after editing the template:
//
//   npm i -D ttsc && npx typia generate --input scripts --output scripts
//
import * as _assertGuard_1 from "typia/lib/internal/_assertGuard";
import * as _jsonStringifyNumber_1 from "typia/lib/internal/_jsonStringifyNumber";
import * as _jsonStringifyString_1 from "typia/lib/internal/_jsonStringifyString";
import * as _jsonStringifyArray_1 from "typia/lib/internal/_jsonStringifyArray";
import * as _throwTypeGuardError_1 from "typia/lib/internal/_throwTypeGuardError";
import * as _accessExpressionAsString_1 from "typia/lib/internal/_accessExpressionAsString";
const __typia_transform__accessExpressionAsString = _accessExpressionAsString_1._accessExpressionAsString;
import typia from "typia";
export interface User {
    id: number;
    name: string;
    email: string;
    age: number;
    verified: boolean;
    score: number;
    role: string;
}
export const encUser = (v: User) => (() => {
    const _ao0 = (input: any, _path: string, _exceptionable: boolean = true): boolean => ("number" === typeof input.id && Number.isFinite(input.id) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".id",
        expected: "number",
        value: input.id
    }, _errorFactory)) && ("string" === typeof input.name || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".name",
        expected: "string",
        value: input.name
    }, _errorFactory)) && ("string" === typeof input.email || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".email",
        expected: "string",
        value: input.email
    }, _errorFactory)) && ("number" === typeof input.age && Number.isFinite(input.age) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".age",
        expected: "number",
        value: input.age
    }, _errorFactory)) && ("boolean" === typeof input.verified || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".verified",
        expected: "boolean",
        value: input.verified
    }, _errorFactory)) && ("number" === typeof input.score && Number.isFinite(input.score) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".score",
        expected: "number",
        value: input.score
    }, _errorFactory)) && ("string" === typeof input.role || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".role",
        expected: "string",
        value: input.role
    }, _errorFactory));
    const _io0 = (input: any): boolean => "number" === typeof input.id && Number.isFinite(input.id) && "string" === typeof input.name && "string" === typeof input.email && ("number" === typeof input.age && Number.isFinite(input.age)) && "boolean" === typeof input.verified && ("number" === typeof input.score && Number.isFinite(input.score)) && "string" === typeof input.role;
    const _so0 = (input: any): any => `{"id":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.id))},"name":${_jsonStringifyString_1._jsonStringifyString(input.name)},"email":${_jsonStringifyString_1._jsonStringifyString(input.email)},"age":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.age))},"verified":${String(input.verified)},"score":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.score))},"role":${_jsonStringifyString_1._jsonStringifyString(input.role)}}`;
    const __is = (input: any): input is User => "object" === typeof input && null !== input && _io0(input);
    let _errorFactory: any;
    const __assert = (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): User => {
        if (false === __is(input)) {
            _errorFactory = errorFactory;
            ((input: any, _path: string, _exceptionable: boolean = true) => ("object" === typeof input && null !== input || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "User",
                value: input
            }, _errorFactory)) && _ao0(input, _path + "", true) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "User",
                value: input
            }, _errorFactory))(input, "$input", true);
        }
        return input;
    };
    const __stringify = (input: User): string => _so0(input);
    return (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): string => {
        __assert(input, errorFactory);
        return __stringify(input);
    };
})()(v);
export interface Row {
    id: number;
    name: string;
    active: boolean;
}
export const encRows = (v: Row[]) => (() => {
    const _ao0 = (input: any, _path: string, _exceptionable: boolean = true): boolean => ("number" === typeof input.id && Number.isFinite(input.id) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".id",
        expected: "number",
        value: input.id
    }, _errorFactory)) && ("string" === typeof input.name || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".name",
        expected: "string",
        value: input.name
    }, _errorFactory)) && ("boolean" === typeof input.active || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".active",
        expected: "boolean",
        value: input.active
    }, _errorFactory));
    const _io0 = (input: any): boolean => "number" === typeof input.id && Number.isFinite(input.id) && "string" === typeof input.name && "boolean" === typeof input.active;
    const _so0 = (input: any): any => `{"id":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.id))},"name":${_jsonStringifyString_1._jsonStringifyString(input.name)},"active":${String(input.active)}}`;
    const __is = (input: any): input is Row[] => Array.isArray(input) && input.every((elem: any) => "object" === typeof elem && null !== elem && _io0(elem));
    let _errorFactory: any;
    const __assert = (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): Row[] => {
        if (false === __is(input)) {
            _errorFactory = errorFactory;
            ((input: any, _path: string, _exceptionable: boolean = true) => (Array.isArray(input) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Array<Row>",
                value: input
            }, _errorFactory)) && input.every((elem: any, _index2: number) => ("object" === typeof elem && null !== elem || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "[" + _index2 + "]",
                expected: "Row",
                value: elem
            }, _errorFactory)) && _ao0(elem, _path + "[" + _index2 + "]", true) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "[" + _index2 + "]",
                expected: "Row",
                value: elem
            }, _errorFactory)) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Array<Row>",
                value: input
            }, _errorFactory))(input, "$input", true);
        }
        return input;
    };
    const __stringify = (input: Row[]): string => `[${_jsonStringifyArray_1._jsonStringifyArray(input, (elem: any) => _so0(elem))}]`;
    return (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): string => {
        __assert(input, errorFactory);
        return __stringify(input);
    };
})()(v);
export type Ev = {
    type: "click";
    x: number;
    y: number;
} | {
    type: "view";
    path: string;
} | {
    type: "error";
    message: string;
    code: number;
};
export interface Feed {
    events: Ev[];
}
export const encFeed = (v: Feed) => (() => {
    const _ae0 = "({ type: \"click\"; x: number; y: number; } | { type: \"error\"; message: string; code: number; } | { type: \"view\"; path: string; })";
    const _ae1 = "({ type: \"click\"; x: number; y: number; } | { type: \"view\"; path: string; } | { type: \"error\"; message: string; code: number; })";
    const _ao0 = (input: any, _path: string, _exceptionable: boolean = true): boolean => (Array.isArray(input.events) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".events",
        expected: "Array<Ev>",
        value: input.events
    }, _errorFactory)) && input.events.every((elem: any, _index2: number) => ("object" === typeof elem && null !== elem || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".events[" + _index2 + "]",
        expected: _ae0,
        value: elem
    }, _errorFactory)) && _au0(elem, _path + ".events[" + _index2 + "]", true && _exceptionable) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".events[" + _index2 + "]",
        expected: _ae0,
        value: elem
    }, _errorFactory)) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".events",
        expected: "Array<Ev>",
        value: input.events
    }, _errorFactory);
    const _ao1 = (input: any, _path: string, _exceptionable: boolean = true): boolean => ("click" === input.type || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".type",
        expected: "\"click\"",
        value: input.type
    }, _errorFactory)) && ("number" === typeof input.x && Number.isFinite(input.x) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".x",
        expected: "number",
        value: input.x
    }, _errorFactory)) && ("number" === typeof input.y && Number.isFinite(input.y) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".y",
        expected: "number",
        value: input.y
    }, _errorFactory));
    const _ao2 = (input: any, _path: string, _exceptionable: boolean = true): boolean => ("view" === input.type || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".type",
        expected: "\"view\"",
        value: input.type
    }, _errorFactory)) && ("string" === typeof input.path || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".path",
        expected: "string",
        value: input.path
    }, _errorFactory));
    const _ao3 = (input: any, _path: string, _exceptionable: boolean = true): boolean => ("error" === input.type || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".type",
        expected: "\"error\"",
        value: input.type
    }, _errorFactory)) && ("string" === typeof input.message || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".message",
        expected: "string",
        value: input.message
    }, _errorFactory)) && ("number" === typeof input.code && Number.isFinite(input.code) || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".code",
        expected: "number",
        value: input.code
    }, _errorFactory));
    const _au0 = (input: any, _path: string, _exceptionable: boolean = true): any => (() => {
        if ("click" === input.type)
            return _ao1(input, _path, true && _exceptionable);
        else if ("view" === input.type)
            return _ao2(input, _path, true && _exceptionable);
        else if ("error" === input.type)
            return _ao3(input, _path, true && _exceptionable);
        else
            return _assertGuard_1._assertGuard(_exceptionable, {
                method: "typia.json.assertStringify",
                path: _path,
                expected: _ae1,
                value: input
            }, _errorFactory);
    })();
    const _io0 = (input: any): boolean => Array.isArray(input.events) && input.events.every((elem: any) => "object" === typeof elem && null !== elem && _iu0(elem));
    const _io1 = (input: any): boolean => "click" === input.type && ("number" === typeof input.x && Number.isFinite(input.x)) && ("number" === typeof input.y && Number.isFinite(input.y));
    const _io2 = (input: any): boolean => "view" === input.type && "string" === typeof input.path;
    const _io3 = (input: any): boolean => "error" === input.type && "string" === typeof input.message && ("number" === typeof input.code && Number.isFinite(input.code));
    const _iu0 = (input: any): any => (() => {
        if ("click" === input.type)
            return _io1(input);
        else if ("view" === input.type)
            return _io2(input);
        else if ("error" === input.type)
            return _io3(input);
        else
            return false;
    })();
    const _so0 = (input: any): any => `{"events":${`[${_jsonStringifyArray_1._jsonStringifyArray(input.events, (elem: any) => _su0(elem))}]`}}`;
    const _so1 = (input: any): any => `{"type":${"\"" + input.type + "\""},"x":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.x))},"y":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.y))}}`;
    const _so2 = (input: any): any => `{"type":${"\"" + input.type + "\""},"path":${_jsonStringifyString_1._jsonStringifyString(input.path)}}`;
    const _so3 = (input: any): any => `{"type":${"\"" + input.type + "\""},"message":${_jsonStringifyString_1._jsonStringifyString(input.message)},"code":${String(_jsonStringifyNumber_1._jsonStringifyNumber(input.code))}}`;
    const _su0 = (input: any): any => (() => {
        if ("click" === input.type)
            return _so1(input);
        else if ("view" === input.type)
            return _so2(input);
        else if ("error" === input.type)
            return _so3(input);
        else
            _throwTypeGuardError_1._throwTypeGuardError({
                method: "typia.json.assertStringify",
                expected: "({ type: \"click\"; x: number; y: number; } | { type: \"view\"; path: string; } | { type: \"error\"; message: string; code: number; })",
                value: input
            });
    })();
    const __is = (input: any): input is Feed => "object" === typeof input && null !== input && _io0(input);
    let _errorFactory: any;
    const _sio1 = (input: any): boolean => "click" === input.type && "number" === typeof input.x && "number" === typeof input.y;
    const _sio2 = (input: any): boolean => "view" === input.type && "string" === typeof input.path;
    const _sio3 = (input: any): boolean => "error" === input.type && "string" === typeof input.message && "number" === typeof input.code;
    const _siu0 = (input: any): any => (() => {
        if ("click" === input.type)
            return _sio1(input);
        else if ("view" === input.type)
            return _sio2(input);
        else if ("error" === input.type)
            return _sio3(input);
        else
            return false;
    })();
    const __assert = (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): Feed => {
        if (false === __is(input)) {
            _errorFactory = errorFactory;
            ((input: any, _path: string, _exceptionable: boolean = true) => ("object" === typeof input && null !== input || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Feed",
                value: input
            }, _errorFactory)) && _ao0(input, _path + "", true) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Feed",
                value: input
            }, _errorFactory))(input, "$input", true);
        }
        return input;
    };
    const __stringify = (input: Feed): string => _so0(input);
    return (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): string => {
        __assert(input, errorFactory);
        return __stringify(input);
    };
})()(v);
export const encNumDict = (v: Record<string, number>) => (() => {
    const _ao0 = (input: any, _path: string, _exceptionable: boolean = true): boolean => false === _exceptionable || Object.keys(input).every((key: any) => {
        const value = input[key];
        if (undefined === value)
            return true;
        return "number" === typeof value && Number.isFinite(value) || _assertGuard_1._assertGuard(_exceptionable, {
            method: "typia.json.assertStringify",
            path: _path + __typia_transform__accessExpressionAsString(key),
            expected: "number",
            value: value
        }, _errorFactory);
    });
    const _io0 = (input: any): boolean => Object.keys(input).every((key: any) => {
        const value = input[key];
        if (undefined === value)
            return true;
        return "number" === typeof value && Number.isFinite(value);
    });
    const _so0 = (input: any): any => `{${Object.entries(input).map(([key, value]: [string, any]) => { if (undefined === value)
        return ""; return `${`${JSON.stringify(key)}:`}${String(_jsonStringifyNumber_1._jsonStringifyNumber(value))}`; }).filter((str: any) => "" !== str).join(",")}}`;
    const __is = (input: any): input is Record<string, number> => "object" === typeof input && null !== input && false === Array.isArray(input) && _io0(input);
    let _errorFactory: any;
    const __assert = (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): Record<string, number> => {
        if (false === __is(input)) {
            _errorFactory = errorFactory;
            ((input: any, _path: string, _exceptionable: boolean = true) => ("object" === typeof input && null !== input && false === Array.isArray(input) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Record<string, number>",
                value: input
            }, _errorFactory)) && _ao0(input, _path + "", true) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Record<string, number>",
                value: input
            }, _errorFactory))(input, "$input", true);
        }
        return input;
    };
    const __stringify = (input: Record<string, number>): string => _so0(input);
    return (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): string => {
        __assert(input, errorFactory);
        return __stringify(input);
    };
})()(v);
export const encStrDict = (v: Record<string, string>) => (() => {
    const _ao0 = (input: any, _path: string, _exceptionable: boolean = true): boolean => false === _exceptionable || Object.keys(input).every((key: any) => {
        const value = input[key];
        if (undefined === value)
            return true;
        return "string" === typeof value || _assertGuard_1._assertGuard(_exceptionable, {
            method: "typia.json.assertStringify",
            path: _path + __typia_transform__accessExpressionAsString(key),
            expected: "string",
            value: value
        }, _errorFactory);
    });
    const _io0 = (input: any): boolean => Object.keys(input).every((key: any) => {
        const value = input[key];
        if (undefined === value)
            return true;
        return "string" === typeof value;
    });
    const _so0 = (input: any): any => `{${Object.entries(input).map(([key, value]: [string, any]) => { if (undefined === value)
        return ""; return `${`${JSON.stringify(key)}:`}${_jsonStringifyString_1._jsonStringifyString(value)}`; }).filter((str: any) => "" !== str).join(",")}}`;
    const __is = (input: any): input is Record<string, string> => "object" === typeof input && null !== input && false === Array.isArray(input) && _io0(input);
    let _errorFactory: any;
    const __assert = (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): Record<string, string> => {
        if (false === __is(input)) {
            _errorFactory = errorFactory;
            ((input: any, _path: string, _exceptionable: boolean = true) => ("object" === typeof input && null !== input && false === Array.isArray(input) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Record<string, string>",
                value: input
            }, _errorFactory)) && _ao0(input, _path + "", true) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "Record<string, string>",
                value: input
            }, _errorFactory))(input, "$input", true);
        }
        return input;
    };
    const __stringify = (input: Record<string, string>): string => _so0(input);
    return (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): string => {
        __assert(input, errorFactory);
        return __stringify(input);
    };
})()(v);
export interface WireEvent {
    id: string;
    payload: string;
    createdAt: string;
    label: string;
}
export const encWire = (v: WireEvent) => (() => {
    const _ao0 = (input: any, _path: string, _exceptionable: boolean = true): boolean => ("string" === typeof input.id || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".id",
        expected: "string",
        value: input.id
    }, _errorFactory)) && ("string" === typeof input.payload || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".payload",
        expected: "string",
        value: input.payload
    }, _errorFactory)) && ("string" === typeof input.createdAt || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".createdAt",
        expected: "string",
        value: input.createdAt
    }, _errorFactory)) && ("string" === typeof input.label || _assertGuard_1._assertGuard(_exceptionable, {
        method: "typia.json.assertStringify",
        path: _path + ".label",
        expected: "string",
        value: input.label
    }, _errorFactory));
    const _io0 = (input: any): boolean => "string" === typeof input.id && "string" === typeof input.payload && "string" === typeof input.createdAt && "string" === typeof input.label;
    const _so0 = (input: any): any => `{"id":${_jsonStringifyString_1._jsonStringifyString(input.id)},"payload":${_jsonStringifyString_1._jsonStringifyString(input.payload)},"createdAt":${_jsonStringifyString_1._jsonStringifyString(input.createdAt)},"label":${_jsonStringifyString_1._jsonStringifyString(input.label)}}`;
    const __is = (input: any): input is WireEvent => "object" === typeof input && null !== input && _io0(input);
    let _errorFactory: any;
    const __assert = (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): WireEvent => {
        if (false === __is(input)) {
            _errorFactory = errorFactory;
            ((input: any, _path: string, _exceptionable: boolean = true) => ("object" === typeof input && null !== input || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "WireEvent",
                value: input
            }, _errorFactory)) && _ao0(input, _path + "", true) || _assertGuard_1._assertGuard(true, {
                method: "typia.json.assertStringify",
                path: _path + "",
                expected: "WireEvent",
                value: input
            }, _errorFactory))(input, "$input", true);
        }
        return input;
    };
    const __stringify = (input: WireEvent): string => _so0(input);
    return (input: any, errorFactory?: (p: import("typia").TypeGuardError.IProps) => Error): string => {
        __assert(input, errorFactory);
        return __stringify(input);
    };
})()(v);
