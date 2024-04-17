"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.setItemInStorage = exports.getItemFromStorage = exports.getDataAsync = exports.postDataAsync = exports.hideElement = exports.showElement = void 0;
function showElement(elementId) {
    var element = document.getElementById(elementId);
    if (element) {
        element.style.display = "block";
    }
}
exports.showElement = showElement;
function hideElement(elementId) {
    var element = document.getElementById(elementId);
    if (element) {
        element.style.display = "none";
    }
}
exports.hideElement = hideElement;
function postDataAsync() {
    return __awaiter(this, arguments, void 0, function (url, data) {
        var myHeaders, d, requestOptions, response, error_1;
        if (url === void 0) { url = ""; }
        if (data === void 0) { data = {}; }
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    _a.trys.push([0, 2, , 3]);
                    myHeaders = new Headers();
                    myHeaders.append("Content-Type", "application/json");
                    d = JSON.stringify(data);
                    requestOptions = {
                        credentials: "same-origin",
                        method: 'POST',
                        headers: myHeaders,
                        body: d,
                        redirect: 'follow'
                    };
                    return [4 /*yield*/, fetch(url, requestOptions)];
                case 1:
                    response = _a.sent();
                    if (response.status == 409) {
                        return [2 /*return*/, { result: "error", reason: "user_already_exists" }];
                    }
                    return [2 /*return*/, response.json()];
                case 2:
                    error_1 = _a.sent();
                    console.error(error_1);
                    return [3 /*break*/, 3];
                case 3: return [2 /*return*/];
            }
        });
    });
}
exports.postDataAsync = postDataAsync;
function getDataAsync() {
    return __awaiter(this, arguments, void 0, function (url) {
        var response;
        if (url === void 0) { url = ""; }
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log(url);
                    return [4 /*yield*/, fetch(url, {
                            method: "GET", // *GET, POST, PUT, DELETE, etc.
                            //   mode: "no-cors", // no-cors, *cors, same-origin
                            cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
                            credentials: "same-origin", // include, *same-origin, omit
                            redirect: "follow", // manual, *follow, error
                            referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
                        })];
                case 1:
                    response = _a.sent();
                    console.log(response);
                    return [2 /*return*/, response.json()];
            }
        });
    });
}
exports.getDataAsync = getDataAsync;
function getItemFromStorage(Key) {
    var item = localStorage.getItem(Key);
    console.log(item);
    if (item == null || item == "undefined") {
        return null;
    }
    return JSON.parse(item);
}
exports.getItemFromStorage = getItemFromStorage;
function setItemInStorage(Key, Value) { localStorage.setItem(Key, JSON.stringify(Value)); }
exports.setItemInStorage = setItemInStorage;
