use super::run_checks;

#[test]
fn webpack_4_bootstrap() {
    let res = run_checks(
        r###"
        var array = []; // we'll be pushing to this

        /******/ (function(modules) { // webpackBootstrap
        /******/ 	// The module cache
        /******/ 	var installedModules = {};
        /******/
        /******/ 	// The require function
        /******/ 	function __webpack_require__(moduleId) {
        /******/
        /******/ 		// Check if module is in cache
        /******/ 		if(installedModules[moduleId]) {
        /******/ 			return installedModules[moduleId].exports;
        /******/ 		}
        /******/ 		// Create a new module (and put it into the cache)
        /******/ 		var module = installedModules[moduleId] = {
        /******/ 			i: moduleId,
        /******/ 			l: false,
        /******/ 			exports: {}
        /******/ 		};
        /******/
        /******/ 		// Execute the module function
        /******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
        /******/
        /******/ 		// Flag the module as loaded
        /******/ 		module.l = true;
        /******/
        /******/ 		// Return the exports of the module
        /******/ 		return module.exports;
        /******/ 	}
        /******/
        /******/
        /******/ 	// expose the modules object (__webpack_modules__)
        /******/ 	__webpack_require__.m = modules;
        /******/
        /******/ 	// expose the module cache
        /******/ 	__webpack_require__.c = installedModules;
        /******/
        /******/ 	// define getter function for harmony exports
        /******/ 	__webpack_require__.d = function(exports, name, getter) {
        /******/ 		if(!__webpack_require__.o(exports, name)) {
        /******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
        /******/ 		}
        /******/ 	};
        /******/
        /******/ 	// define __esModule on exports
        /******/ 	__webpack_require__.r = function(exports) {
        /******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
        /******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
        /******/ 		}
        /******/ 		Object.defineProperty(exports, '__esModule', { value: true });
        /******/ 	};
        /******/
        /******/ 	// create a fake namespace object
        /******/ 	// mode & 1: value is a module id, require it
        /******/ 	// mode & 2: merge all properties of value into the ns
        /******/ 	// mode & 4: return value when already ns object
        /******/ 	// mode & 8|1: behave like require
        /******/ 	__webpack_require__.t = function(value, mode) {
        /******/ 		if(mode & 1) value = __webpack_require__(value);
        /******/ 		if(mode & 8) return value;
        /******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
        /******/ 		var ns = Object.create(null);
        /******/ 		__webpack_require__.r(ns);
        /******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
        /******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
        /******/ 		return ns;
        /******/ 	};
        /******/
        /******/ 	// getDefaultExport function for compatibility with non-harmony modules
        /******/ 	__webpack_require__.n = function(module) {
        /******/ 		var getter = module && module.__esModule ?
        /******/ 			function getDefault() { return module['default']; } :
        /******/ 			function getModuleExports() { return module; };
        /******/ 		__webpack_require__.d(getter, 'a', getter);
        /******/ 		return getter;
        /******/ 	};
        /******/
        /******/ 	// Object.prototype.hasOwnProperty.call
        /******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
        /******/
        /******/ 	// __webpack_public_path__
        /******/ 	__webpack_require__.p = "/dev-to-b/";
        /******/
        /******/
        /******/ 	// Load entry module and return exports
        /******/ 	return __webpack_require__(__webpack_require__.s = 1);
        /******/ })
        /************************************************************************/
        /******/ ([
        /* 0 */
        /***/ (function(module, exports) {

        array.push("loaded module once");

        /***/ }),
        /* 1 */
        /***/ (function(module, exports, __webpack_require__) {

        __webpack_require__(0);
        __webpack_require__(0);

        /***/ })
        /******/ ]);

        return array;
        "###,
    );
    insta::assert_display_snapshot!(res, @r###"["loaded module once"]"###);
}

#[test]
fn webpack_5_bootstrap() {
    let res = run_checks(
        r###"
        var array = []; // we'll be pushing to this

        /******/ (() => { // webpackBootstrap
        /******/ 	var __webpack_modules__ = ({

        /***/ 520:
        /***/ (() => {

        array.push("loaded module once");

        /***/ })

        /******/ 	});
        /************************************************************************/
        /******/ 	// The module cache
        /******/ 	var __webpack_module_cache__ = {};
        /******/ 	
        /******/ 	// The require function
        /******/ 	function __webpack_require__(moduleId) {
        /******/ 		// Check if module is in cache
        /******/ 		var cachedModule = __webpack_module_cache__[moduleId];
        /******/ 		if (cachedModule !== undefined) {
        /******/ 			return cachedModule.exports;
        /******/ 		}
        /******/ 		// Create a new module (and put it into the cache)
        /******/ 		var module = __webpack_module_cache__[moduleId] = {
        /******/ 			// no module.id needed
        /******/ 			// no module.loaded needed
        /******/ 			exports: {}
        /******/ 		};
        /******/ 	
        /******/ 		// Execute the module function
        /******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
        /******/ 	
        /******/ 		// Return the exports of the module
        /******/ 		return module.exports;
        /******/ 	}
        /******/ 	
        /************************************************************************/

        var __webpack_exports__ = {};
        // This entry need to be wrapped in an IIFE because it need to be isolated against other modules in the chunk.
        (() => {
        __webpack_require__(520);
        __webpack_require__(520);

        })();

        /******/ })();

        return array;
        "###,
    );
    insta::assert_display_snapshot!(res, @r###"["loaded module once"]"###);
}
