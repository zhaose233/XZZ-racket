# XZZ的racket不完全重实现

- 使用racket重写了XZZ插件框架，适用于go-cqhttp
- 一些基础的配置在config.json中
- 要写插件的话参照plugins里面的文件写好插件的主函数后，在router.rkt里面引用新插件，注册触发字符串
