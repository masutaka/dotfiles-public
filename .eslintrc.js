module.exports = {
    "env": {
      "browser": true,
      "es6": true,
      "jasmine": true,
      "jquery": true
    },
    "extends": "eslint:recommended",
    "rules": {
        "indent": [
            "error",
            2
        ],
        "linebreak-style": [
            "error",
            "unix"
        ],
        "quotes": [
            "error",
            "single"
        ],
        "semi": [
            "error",
            "always"
        ],
        "no-console": [
            "error", {
	      allow: ["log"]
	    }
        ]
    }
};
