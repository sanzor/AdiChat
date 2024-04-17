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
var elements_1 = require("./elements");
var bus_1 = require("./bus");
var utils_1 = require("./utils");
var elements_2 = require("./elements");
var utils_2 = require("./utils");
var events_1 = require("./events");
var constants_1 = require("./constants");
(0, bus_1.subscribeToEvent)(events_1.SHOW_REGISTER, onShowRegister);
(0, bus_1.subscribeToEvent)(events_1.HIDE_REGISTER, onHideRegister);
elements_2.submitBtn.addEventListener("click", onSubmit);
elements_2.backToLoginBtn.addEventListener("click", onBackToLogin);
function onShowRegister(_) {
    console.log("called register");
    (0, utils_2.showElement)("registerModal");
}
function onHideRegister(ev) {
    cleanSubmitFailMessage();
    (0, utils_2.hideElement)("registerModal");
}
function cleanSubmitFailMessage() {
    elements_2.registerFailMessage.innerHTML = '';
}
function showSubmitFailMessage(message) {
    elements_2.registerFailMessage.innerHTML = message;
    (0, utils_2.showElement)("registerFailMessage");
}
function onBackToLogin() {
    (0, bus_1.publishEvent)(events_1.HIDE_REGISTER, {});
    (0, bus_1.publishEvent)(events_1.SHOW_LOGIN, {});
}
function onSubmit() {
    return __awaiter(this, void 0, void 0, function () {
        var userData, validateResult, err, succesfulData, userResult, error_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("onSubmit");
                    userData = getCreateUserData();
                    console.log("User data for creating:".concat(userData));
                    validateResult = validateCreateUserData(userData);
                    if (validateResult instanceof Error) {
                        err = validateResult;
                        showSubmitFailMessage("Invalid data having reason:".concat(err.message));
                        return [2 /*return*/];
                    }
                    succesfulData = validateResult;
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 3, , 4]);
                    return [4 /*yield*/, createUserAsync(succesfulData)];
                case 2:
                    userResult = _a.sent();
                    if (userResult.result == "error") {
                        handleUserCreateError(new Error(userResult.result));
                        return [2 /*return*/];
                    }
                    console.log("User created:".concat(userResult));
                    (0, utils_1.setItemInStorage)(constants_1.USER, userResult.result);
                    (0, bus_1.publishEvent)(events_1.HIDE_REGISTER, {});
                    (0, bus_1.publishEvent)(events_1.SHOW_MAIN, {});
                    return [3 /*break*/, 4];
                case 3:
                    error_1 = _a.sent();
                    showSubmitFailMessage(error_1.message);
                    return [3 /*break*/, 4];
                case 4: return [2 /*return*/];
            }
        });
    });
}
function getCreateUserData() {
    var userData = {
        email: elements_1.emailBox.value,
        password: elements_1.passwordBox.value,
        retypePassword: elements_1.retypePasswordBox.value,
        name: elements_1.usernameBox.value
    };
    return userData;
}
function validateCreateUserData(data) {
    console.log(data);
    if (data.name == undefined || data.name == null) {
        return new Error("Invalid username");
    }
    if (data.password == undefined || data.password == null) {
        return new Error("Invalid password");
    }
    if (data.password != data.retypePassword) {
        return new Error("Passwords do not match");
    }
    return data;
}
function createUserAsync(userData) {
    return __awaiter(this, void 0, void 0, function () {
        var url, result;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    url = "".concat(config_1.default.baseHttpUrl, "/create-user");
                    console.log(url);
                    return [4 /*yield*/, (0, utils_1.postDataAsync)(url, userData)];
                case 1:
                    result = _a.sent();
                    console.log("Result create user\n:");
                    console.log(result);
                    return [2 /*return*/, result];
            }
        });
    });
}
function handleUserCreateError(userResult) {
    console.log("Inside handling user error ".concat(userResult));
    elements_2.registerFailMessage.innerHTML = userResult.message;
    return;
}
