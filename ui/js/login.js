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
var config_1 = require("./config");
var utils_1 = require("./utils");
var elements_1 = require("./elements");
var bus_1 = require("./bus");
var utils_2 = require("./utils");
elements_1.loginButton.addEventListener("click", onLogin);
elements_1.registerBtn.addEventListener("click", onRegister);
(0, bus_1.subscribeToEvent)("DOMContentLoaded", onDomContentLoaded);
window.onload = function () {
    return __awaiter(this, void 0, void 0, function () {
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("onload");
                    return [4 /*yield*/, startLoginFlow()];
                case 1:
                    _a.sent();
                    return [2 /*return*/];
            }
        });
    });
};
(0, bus_1.subscribeToEvent)("showLogin", onShowLogin);
(0, bus_1.subscribeToEvent)("hideLogin", onHideLogin);
function onDomContentLoaded() {
    return __awaiter(this, void 0, void 0, function () {
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("ondomcontentloaded");
                    return [4 /*yield*/, startLoginFlow()];
                case 1:
                    _a.sent();
                    return [2 /*return*/];
            }
        });
    });
}
function startLoginFlow() {
    return __awaiter(this, void 0, void 0, function () {
        var user;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("start login flow");
                    user = (0, utils_1.getItemFromStorage)("user");
                    if (!user) {
                        (0, bus_1.publishEvent)("showLogin", {});
                        return [2 /*return*/];
                    }
                    return [4 /*yield*/, tryLoginAsync(user)];
                case 1:
                    _a.sent();
                    return [2 /*return*/];
            }
        });
    });
}
function onShowLogin(ev) {
    (0, utils_2.showElement)("loginModal");
}
function onHideLogin(ev) {
    (0, utils_2.hideElement)("loginModal");
}
function tryLoginAsync(user) {
    return __awaiter(this, void 0, void 0, function () {
        var userResult, error_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("try login");
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 3, , 4]);
                    console.log(user.id);
                    return [4 /*yield*/, getUserByIdAsync(user.id)];
                case 2:
                    userResult = _a.sent();
                    if (userResult) {
                        (0, bus_1.publishEvent)("showMain", {});
                        return [2 /*return*/];
                    }
                    console.log("Could not log in with the user ");
                    (0, bus_1.publishEvent)("showLogin", {});
                    return [3 /*break*/, 4];
                case 3:
                    error_1 = _a.sent();
                    console.log("Error at try login");
                    (0, bus_1.publishEvent)("showLogin", {});
                    return [3 /*break*/, 4];
                case 4: return [2 /*return*/];
            }
        });
    });
}
function onLogin() {
    return __awaiter(this, void 0, void 0, function () {
        var loginResult;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, loginAsync()];
                case 1:
                    loginResult = _a.sent();
                    if (loginResult != true) {
                        showLoginErrorMessage(loginResult.message);
                        return [2 /*return*/];
                    }
                    console.log("\nLogin succesfull for ".concat(localStorage.user, "\n"));
                    (0, bus_1.publishEvent)("hideLogin", {});
                    (0, bus_1.publishEvent)("showMain", {});
                    return [2 /*return*/];
            }
        });
    });
}
function loginAsync() {
    return __awaiter(this, void 0, void 0, function () {
        var user, error_2;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    _a.trys.push([0, 2, , 3]);
                    return [4 /*yield*/, getUserByEmailAsync()];
                case 1:
                    user = _a.sent();
                    console.log(user);
                    localStorage.setItem("user", JSON.stringify(user));
                    return [2 /*return*/, true];
                case 2:
                    error_2 = _a.sent();
                    return [2 /*return*/, error_2];
                case 3: return [2 /*return*/];
            }
        });
    });
}
function onRegister() {
    clearLoginErrorMessage();
    (0, bus_1.publishEvent)("hideLogin", {});
    (0, bus_1.publishEvent)("showRegister", {});
}
function getUserByIdAsync(Id) {
    return __awaiter(this, void 0, void 0, function () {
        var url, result;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    url = "".concat(config_1.default.baseHttpUrl, "/get-user?id=").concat(Id);
                    return [4 /*yield*/, (0, utils_1.getDataAsync)(url)];
                case 1:
                    result = _a.sent();
                    return [2 /*return*/, result];
            }
        });
    });
}
function getUserByEmailAsync() {
    return __awaiter(this, void 0, void 0, function () {
        var email, password, url, result;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    email = elements_1.emailLoginBox.value;
                    password = elements_1.passwordLoginBox.value;
                    url = "".concat(config_1.default.baseHttpUrl, "/get-user-by-email?email=").concat(email, "&password=").concat(password);
                    return [4 /*yield*/, (0, utils_1.getDataAsync)(url)];
                case 1:
                    result = _a.sent();
                    console.log(result);
                    return [2 /*return*/, result];
            }
        });
    });
}
function clearLoginErrorMessage() {
    elements_1.loginFailMessage.innerHTML = '';
    elements_1.loginFailMessage.style.display = "none";
}
function showLoginErrorMessage(message) {
    console.log("Inside show login error message");
    elements_1.loginFailMessage.innerHTML = "Could not login. Reason:".concat(message);
    elements_1.loginFailMessage.style.display = "block";
}
