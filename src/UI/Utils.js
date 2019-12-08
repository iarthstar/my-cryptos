"use strict";

exports.showSnackbar = ({ message, timeout, actionHandler, actionText }) => () => {
    document.getElementById('snack-bar').MaterialSnackbar.showSnackbar({
        message,
        timeout,
        actionHandler: actionHandler.hasOwnProperty('value0') ? actionHandler['value0']() : undefined,
        actionText: actionText.hasOwnProperty('value0') ? actionText['value0'] : undefined,
    });
}

exports.showLoader = () => document.getElementById('loader').style.display = 'flex';

exports.hideLoader = () => document.getElementById('loader').style.display = 'none';

exports.isEqual = (s1) => (s2) => s1.valueOf() === s2.valueOf();
